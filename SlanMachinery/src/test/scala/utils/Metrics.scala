package utils      // pick any package you like

object Metrics {
  case class Sample(name: String, millis: Long, memBytes: Long)
  private var samples = Vector.empty[Sample]

  private def usedMem: Long = {
    val rt = Runtime.getRuntime
    rt.totalMemory - rt.freeMemory
  }

  def measure(label: String)(body: => Unit): Unit = {
    System.gc(); Thread.sleep(50)
    val beforeMem = usedMem
    val t0 = System.nanoTime()
    body
    val durMs = (System.nanoTime() - t0) / 1_000_000
    System.gc(); Thread.sleep(50)
    val afterMem = usedMem
    samples :+= Sample(label, durMs, math.max(0L, afterMem - beforeMem))
    println(f"[METRIC] $label%-30s  time=$durMs%6d ms   mem=${(afterMem-beforeMem)/1024}%,10d KB ")
  }

  def report(): Unit = {
    println("\\n======== DSL‑spec metrics ========")
    samples.foreach { s =>
      println(f"${s.name}%-30s  ${s.millis}%6d ms   ${s.memBytes/1024}%,10d KB ")
    }
  }
}
