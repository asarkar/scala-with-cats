package ch11

trait KeyValueStore[F[_, _]]:
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]

object KeyValueStore:
  given KeyValueStore[Map] with
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
      f + (k -> v)

    def get[K, V](f: Map[K, V])(k: K): Option[V] =
      f.get(k)

    override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
      f.getOrElse(k, default)

    def values[K, V](f: Map[K, V]): List[V] =
      f.values.toList

object KeyValueStoreSyntax:
  extension [F[_, _], K, V](f: F[K, V])(using kvs: KeyValueStore[F])
    def put(key: K, value: V) =
      kvs.put(f)(key, value)

    def get(key: K): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V): V =
      kvs.getOrElse(f)(key, default)

    def values: List[V] =
      kvs.values(f)
