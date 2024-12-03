package scalang.util.concurrent;

import java.util.AbstractSet;
import java.util.concurrent.ConcurrentHashMap;

class ConcurrentHashSet[E] extends AbstractSet[E] {

  val V = "";

  val map = new ConcurrentHashMap[E,Object]

  override def add(obj: E) = {
    map.putIfAbsent(obj, V) == null;
  }

  override def contains(obj: Object) = {
    map.containsKey(obj);
  }

  override def remove(obj: Object) = {
    map.remove(obj) == V;
  }

  override def size() = {
    map.size();
  }

  override def clear() {
    map.clear();
  }

  override def iterator() = {
    map.keySet().iterator();
  }

}
