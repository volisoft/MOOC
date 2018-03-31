val w = 6
val step = 4

val mod = w % step

val r = 0 to w by step
val intervals =
  if (mod == 0) r.zip(r.tail)
  else r.zip(r.tail) :+ (w - mod, w)
