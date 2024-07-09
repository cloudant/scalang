package scalang

object Poller {

  type MilliSec = Long

  val defaultTimeout = 5000
  val defaultSleepInterval = 50

  def waitFor(
    condition : () => Boolean,
    timeout : MilliSec = defaultTimeout,
    sleepInterval : MilliSec = defaultSleepInterval
  ) : Boolean = {
    val limit = now + timeout
    while (now < limit) {
      if (condition()) {
        return true
      } else {
        Thread.sleep(sleepInterval)
      }
    }
    false
  }

  def now : MilliSec = {
    System.currentTimeMillis
  }
}
