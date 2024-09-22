package prelude

def zipAllByKey[K, A, B](a: Map[K, A], b: Map[K, B]): Map[K, (A, B)] =
  a.keysIterator
    .map { key => key -> (a.get(key), b.get(key)).bisequence }
    .collect { case (k, Some(v)) => k -> v }
    .toMap
