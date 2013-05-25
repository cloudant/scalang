package scalang.soak

import org.specs._
import scalang._
import java.lang.{Process => JProc}
import scala.Some

class SoakSpec extends SpecificationWithJUnit {
  "Scalang" should {

    var epmd : JProc = null
    var erl : JProc = null
    var node : ErlangNode = null

    doBefore {
      epmd = EpmdCmd()
    }

    doAfter {
      epmd.destroy
      epmd.waitFor
      if (node != null) { node.shutdown }
      if (erl != null) {
        erl.destroy
        erl.waitFor
      }
    }

    val cookie = "test"

    var counter = 1
    doAfter(counter += 1)
    until(counter == 1000000)

    "start up reliably every time" in {
      println(counter)
      node = Node(Symbol("scala@localhost"), cookie)
      erl = ErlangVM("tmp@localhost", cookie, Some("io:format(\"~p~n\", [net_adm:ping('scala@localhost')])."))
      val result = ReadLine(erl)
      result must ==("pong")
      node.connections.keySet.toSet must contain(Symbol("tmp@localhost"))
    }
  }
}
