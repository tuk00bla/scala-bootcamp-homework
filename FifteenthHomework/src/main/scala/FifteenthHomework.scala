import cats.{Monad, effect}
import cats.Parallel.Aux
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource, _}
import cats.implicits.catsSyntaxParallelTraverse
import cats.syntax.all._

import java.io.File
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.io.{Source, StdIn}
import scala.util.{Random, Success, Try}

object FifteenthHomework extends IOApp {
  implicit val ioConcurrentEffect: ConcurrentEffect[IO] = IO.ioConcurrentEffect
  private val cpuEC: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newCachedThreadPool((r: Runnable) =>
      new Thread(r) { setName(s"cpu-ec-${this.getName}") }
    )
  )
  private val cpuCH = IO.contextShift(cpuEC)
  implicit val cpuParallel: Aux[IO, effect.IO.Par] = IO.ioParallel(cpuCH)
  private val hashCount = 100

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO]
      .use { blocker =>
      {
        for {
          path  <- blocker.delay(readFilePath)
          seed  <- blocker.delay(readSeed)
          text  <- blocker.blockOn(loadFile(path))
          _     <- IO.shift(cpuEC)
          words <- IO(splitToWords(text))
          funcs <- IO(buildHashFunctions(seed))
          sign  <- funcs.parTraverse(fn => IO(calculateMinHash(words, fn)))
          _     <- IO.shift
          storage  <- Storage.inMemory[IO]
          _     <- storage.save(path, Signature(sign))
        } yield sign
      }
      }
      .as(ExitCode.Success)
  }

  @tailrec
  private def readFilePath: String = {
    printThreadInfo("readFilePath")
    print("Enter file path: ")
    val path = StdIn.readLine()
    Try {
      val file = new File(path)
      file.exists() && file.isFile && file.canRead
    } match {
      case Success(true) => path
      case _ => printError("Invalid file path"); readFilePath
    }
  }

  @tailrec
  private def readSeed: Int = {
    printThreadInfo("readSeed")
    print("Enter seed: ")
    val input = StdIn.readLine()
    val seed  = input.toIntOption
    seed match {
      case Some(s) => s
      case None    => printError {
        "Invalid seed"
      }
        readSeed
    }
  }

  private def loadFile(filePath: String): IO[List[String]] =
    Resource
      .fromAutoCloseable(IO({
        printThreadInfo("loadFile->Source.fromFile"); Source.fromFile(filePath)
      }))
      .use(f => IO({ printThreadInfo("loadFile->f.getLines().toList"); f.getLines().toList }))

  private def calculateMinHash(words: Set[String], hash: HashFunction): HashResult = {
    printThreadInfo(s"calculateMinHash(${hash.name})")
    val minHash = words.map(hash.fn).min
    HashResult(hash.name, minHash)
  }

  private def splitToWords(lines: List[String]): Set[String] = {
    printThreadInfo("splitToWords")
    lines.flatMap(_.split("\\s+")).toSet
  }

  private def buildHashFunctions(seed: Int): List[HashFunction] = {
    printThreadInfo("buildHashFunctions")
    val seeds = (1 to hashCount / 2)
      .scanLeft(Seed(seed))((s, _) => s.next)
      .map(_.seed)
    seeds
      .flatMap(s =>
        Array[HashFunction](
          HashFunction(javaHash(s), s"javaHash($s)"),
          HashFunction(knuthHash(s), s"knuthHash($s)")
        )
      )
      .toList
  }

  private def javaHash(seed: Int = 0)(word: String): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  private def knuthHash(constant: Int)(word: String): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }

  def printThreadInfo(op: String): Unit =
    println(
      s"${Console.YELLOW}<<Exec $op on thread ${Thread.currentThread().getName}>>${Console.RESET}"
    )

  def printError(message: String): Unit = {
    println(
      s"${Console.RED}$message${Console.RESET}"
    )
  }

  final case class Seed(seed: Int) {
    lazy val next: Seed = Seed(new Random(seed).nextInt())
  }

  final case class HashFunction(fn: String => Int, name: String)
  final case class HashResult(fnName: String, minHash: Int)
  final case class Signature(signature: List[HashResult])

  trait Storage[F[_]] {
    def save(file: String, signature: Signature): F[Unit]
    def find(file: String): F[Option[Signature]]
  }

  object Storage {
    def inMemory[F[_]: Sync]: F[Storage[F]] = for {
      mapByFile <- Ref.of[F, Map[String, Signature]](Map.empty)
    } yield new InStorage(mapByFile)
  }

  final class InStorage[F[_]: Monad](
                                               mapByFile: Ref[F, Map[String, Signature]]
                                             ) extends Storage[F] {
    override def save(file: String, signature: Signature): F[Unit] = {
      printThreadInfo("InMemory.save")
      mapByFile.update(_.updated(file, signature))
    }

    override def find(file: String): F[Option[Signature]] = mapByFile.get.map(_.get(file))
  }

}