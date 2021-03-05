import scala.collection.immutable.ArraySeq
import scala.collection.mutable


object EighthHomework {

  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }

      object FixedSizeScore {
        val sizeScoreByte = 1
        val sizeScoreChar = 2
        val sizeScoreInt = 4
        val sizeScoreLong = 8

        val sizeObjectHeader = 12
      }
    }

    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        map.put(key, value)
        if (currentSizeScore() > maxSizeScore) {
          val newest = map.drop(map.size - 2)
          map.clear()
          map ++= newest
        }
      }

      def get(key: K): Option[V] = map.get(key)

      private def currentSizeScore(): SizeScore =
        map.keys.map(k => implicitly[GetSizeScore[K]].apply(k)).sum +
          map.values.map(v => implicitly[GetSizeScore[V]].apply(v)).sum
    }


    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }


    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      import syntax._
      import FixedSizeScore._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] =
          f.inner.map { case (k, _) => k }.iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] =
          f.inner.map { case (_, v) => v }.iterator
      }

      implicit val byteSizeScore: GetSizeScore[Byte] = new GetSizeScore[Byte] {
        override def apply(value: Byte): SizeScore = sizeScoreByte
      }

      implicit val charSizeScore: GetSizeScore[Char] = new GetSizeScore[Char] {
        override def apply(value: Char): SizeScore = sizeScoreChar
      }

      implicit val intSizeScore: GetSizeScore[Int] = new GetSizeScore[Int] {
        override def apply(value: Int): SizeScore = sizeScoreInt
      }

      implicit val longSizeScore: GetSizeScore[Long] = new GetSizeScore[Long] {
        override def apply(value: Long): SizeScore = sizeScoreLong
      }

      implicit val stringSizeScore: GetSizeScore[String] = new GetSizeScore[String] {
        override def apply(value: String): SizeScore = sizeObjectHeader + value.length * sizeScoreChar
      }

      implicit def arraySizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = new GetSizeScore[Array[T]] {
        override def apply(value: Array[T]): SizeScore = sizeObjectHeader + value.map(_.sizeScore).sum
      }

      implicit def listSizeScore[T: GetSizeScore]: GetSizeScore[List[T]] = new GetSizeScore[List[T]] {
        override def apply(value: List[T]): SizeScore = sizeObjectHeader + value.map(_.sizeScore).sum
      }

      implicit def vectorSizeScore[T: GetSizeScore]: GetSizeScore[Vector[T]] = new GetSizeScore[Vector[T]] {
        override def apply(value: Vector[T]): SizeScore = sizeObjectHeader + value.map(_.sizeScore).sum
      }

      implicit def mapSizeScore[T: GetSizeScore, V: GetSizeScore]: GetSizeScore[Map[T, V]] = new GetSizeScore[Map[T, V]] {
        override def apply(value: Map[T, V]): SizeScore = {
          (for {
            (k, v) <- value.view
          } yield k.sizeScore + v.sizeScore).sum + sizeObjectHeader

        }
      }

      implicit def packedMapSizeScore[T: GetSizeScore, V: GetSizeScore]: GetSizeScore[PackedMultiMap[T, V]] = new GetSizeScore[PackedMultiMap[T, V]] {
        override def apply(value: PackedMultiMap[T, V]): SizeScore = {
          (for {
            (k, v) <- value.inner
          } yield k.sizeScore + v.sizeScore).sum + sizeObjectHeader
        }
      }
    }
  }


  object MyTwitter {
    import SuperVipCollections4s._
    import instances._
    import syntax._
    import FixedSizeScore._

    final case class Twit(id: Long,
                          userId: Int,
                          hashTags: Vector[String],
                          attributes: PackedMultiMap[String, String],
                          fbiNotes: List[FbiNote])

    final case class FbiNote(month: String,
                             favouriteChar: Char,
                             watchedPewDiePieTimes: Long)

    trait TwitCache {
      def put(twit: Twit): Unit

      def get(id: Long): Option[Twit]
    }

    implicit val fbiNoteSizeScore: GetSizeScore[FbiNote] = new GetSizeScore[FbiNote] {
      override def apply(value: FbiNote): SizeScore =
        value.favouriteChar.sizeScore + value.month.sizeScore + value.watchedPewDiePieTimes.sizeScore + sizeObjectHeader
    }

    implicit val twitSizeScore: GetSizeScore[Twit] = new GetSizeScore[Twit] {
      override def apply(value: Twit): SizeScore =
        value.fbiNotes.sizeScore + value.userId.sizeScore + value.attributes.sizeScore + value.hashTags.sizeScore + value.id.sizeScore + sizeObjectHeader
    }

    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {

      val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = cache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}
