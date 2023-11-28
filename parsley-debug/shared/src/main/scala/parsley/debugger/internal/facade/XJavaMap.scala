package parsley.debugger.internal.facade

import java.util

import org.typelevel.scalaccompat.annotation.unused
import parsley.debugger.internal.XUnsupportedOperationException

private [parsley] trait XJavaMap[K, V] extends util.Map[K, V] with Iterable[(K, V)] {
    import XJavaMap._ // scalastyle:ignore import.grouping underscore.import

    // As specified by the Map.Entry documentation, setValue writes through to the map.
    override def entrySet(): util.Set[util.Map.Entry[K, V]] =
        entrySetS.map { case (k, v) => new util.Map.Entry[K, V] {
                override def getKey: K = k

                override def getValue: V = v

                override def setValue(value: V): V = {
                    val old = getValue
                    put(getKey, value)
                    old
                }
            }
        } =>>= toJavaSet[util.Map.Entry[K, V]]

    // It is recommended that this method is overridden.
    override def size(): Int = iterator.size

    override def putAll(m: util.Map[_ <: K, _ <: V]): Unit =
        (m.entrySet().iterator() =>>= toScalaIterator[util.Map.Entry[_ <: K, _ <: V]]).foreach {
            entry => val _ = put(entry.getKey, entry.getValue): @unused
        }

    override def keySet(): util.Set[K] = keysS =>>= toJavaSet[K]

    override def values(): util.Collection[V] = valuesS =>>= toJavaCollection[V]

    def entrySetS: Set[(K, V)]
    def keysS: Set[K]
    def valuesS: Set[V]

    override def iterator: Iterator[(K, V)] = entrySetS.iterator
}

// Scala's Java collection compatibility layer is not version safe.
// 2.13+ calls it scala.jdk.CollectionConverters._
// 2.12 calls it scala.collection.JavaConverters._
// 2.8 - 2.11 calls it scala.collection.JavaConversions._
// ... Not that below 2.12 matters, but why on earth???
private [facade] object XJavaMap {
    private lazy val nonMutExc: XUnsupportedOperationException =
        new XUnsupportedOperationException("This is not a mutable collection.")

    private [XJavaMap] def toJavaIterator[T](it: Iterator[T]): util.Iterator[T] = new util.Iterator[T] {
        override def hasNext: Boolean = it.hasNext
        override def next(): T = it.next()

    }

    private [XJavaMap] def toScalaIterator[T](it: util.Iterator[T]): Iterator[T] = new Iterator[T] {
        override def hasNext: Boolean = it.hasNext
        override def next(): T = it.next()
    }

    private [XJavaMap] def toScalaIteratorOfAny(it: util.Iterator[_]): Iterator[Any] = new Iterator[Any] {
        override def hasNext: Boolean = it.hasNext
        override def next(): Any = it.next()
    }

    // Unsafe. This assumes T can be cast into U freely.
    private def toArrayOf[T, U](out: Array[U])(in: Iterable[T]): Array[U] = {
        val ua: Array[U] = if (out.length < in.size) new Array[U](in.size) else out
        in.iterator.zipWithIndex.foreach { case (x, ix) => ua(ix) = x.asInstanceOf[U] }
        ua
    }

    // scalastyle:off throw
    private [XJavaMap] def toJavaCollection[T](items: Iterable[T]): util.Collection[T] = new util.Collection[T] {
        override def size(): Int = items.size
        override def isEmpty: Boolean = items.isEmpty
        override def contains(o: Any): Boolean = items.exists(_ == o)
        override def iterator(): util.Iterator[T] = items.iterator =>>= toJavaIterator[T]
        override def toArray: Array[AnyRef] = items.toArray[T].map(_.asInstanceOf[AnyRef])
        override def toArray[U](a: Array[U]): Array[U] = items =>>= toArrayOf[T, U](a)
        override def add(e: T): Boolean = throw nonMutExc.except
        override def remove(o: Any): Boolean = throw nonMutExc.except
        override def containsAll(c: util.Collection[_]): Boolean = (c.iterator() =>>= toScalaIteratorOfAny).forall(contains)
        override def addAll(c: util.Collection[_ <: T]): Boolean = throw nonMutExc.except
        override def removeAll(c: util.Collection[_]): Boolean = throw nonMutExc.except
        override def retainAll(c: util.Collection[_]): Boolean = throw nonMutExc.except
        override def clear(): Unit = throw nonMutExc.except
    }

    // Sets require more specialisation as
    private [XJavaMap] def toJavaSet[T](set: Set[T]): util.Set[T] = new util.Set[T] {
        override def size(): Int = set.size
        override def isEmpty: Boolean = set.isEmpty
        override def contains(o: Any): Boolean = o.isInstanceOf[T] && set.contains(o.asInstanceOf[T])
        override def iterator(): util.Iterator[T] = set.iterator =>>= toJavaIterator[T]
        override def toArray: Array[AnyRef] = set.toArray[T].map(_.asInstanceOf[AnyRef])
        override def toArray[U](a: Array[U]): Array[U] = set =>>= toArrayOf[T, U](a)
        override def add(e: T): Boolean = throw nonMutExc.except
        override def remove(o: Any): Boolean = throw nonMutExc.except
        override def containsAll(c: util.Collection[_]): Boolean = (c.iterator() =>>= toScalaIteratorOfAny).forall(contains)
        override def addAll(c: util.Collection[_ <: T]): Boolean = throw nonMutExc.except
        override def retainAll(c: util.Collection[_]): Boolean = throw nonMutExc.except
        override def removeAll(c: util.Collection[_]): Boolean = throw nonMutExc.except
        override def clear(): Unit = throw nonMutExc.except
    }
    // scalastyle:on throw


    implicit class ConversionOps[T](val item: T) extends AnyVal {
        def =>>=[U](fun: T => U): U = fun(item)
    }
}
