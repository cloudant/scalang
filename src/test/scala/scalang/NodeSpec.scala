package scalang

import org.specs._
import org.specs.runner._
import scalang.node._
import java.lang.{Process => JProc}
import java.io._
import scala.collection.JavaConversions._

class NodeSpec extends SpecificationWithJUnit {
  "Node" should {
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

    val pingTimeout = 1000
    val receiveTimeout = 1000

    "get connections from a remote node" in {
      val nodeName = "scalang_gcfarn@localhost"
      node = Node(Symbol(nodeName), cookie)
      val eval = "io:format(\"~p~n\", [net_kernel:connect_node('" + nodeName + "')])."
      val remoteName = "erlang_gcfarn@localhost"
      erl = ErlangVM(remoteName, cookie, Some(eval))
      val reader = new BufferedReader(new InputStreamReader(erl.getInputStream))
      reader.readLine
      node.channels.keySet.toSet must contain(Symbol(remoteName))
    }

    "connect to a remote node" in {
      val nodeName = "scalang_ctarn@localhost"
      node = Node(Symbol(nodeName), cookie)
      erl = Escript("receive_connection.escript") 
      // start node monitor on node `receive_connection@localhost`
      ReadLine(erl) must ==("ready")
      val erlName = "receive_connection@localhost"
      node.connectAndSend(Symbol(erlName), None)
      val result = ReadLine(erl)
      result must ==(nodeName)
      node.channels.keySet.toSet must contain(Symbol(erlName))
    }

    "accept pings" in {
      val nodeName = "scalang_ap@localhost"
      node = Node(Symbol(nodeName), cookie)
      val eval = "io:format(\"~p~n\", [net_adm:ping('" + nodeName + "')])."
      val remoteName = "erlang_ap@localhost"
      erl = ErlangVM(remoteName, cookie, Some(eval))
      val result = ReadLine(erl)
      result must ==("pong")
      node.channels.keySet.toSet must contain(Symbol(remoteName))
    }

    "send pings" in {
      node = Node(Symbol("scalang_sp@localhost"), cookie)
      // start node monitor on node `receive_connection@localhost`
      erl = Escript("receive_connection.escript")
      ReadLine(erl) must ==("ready")
      node.ping(Symbol("receive_connection@localhost"), pingTimeout) must beTrue
    }

    "invalid pings should fail" in {
      node = Node(Symbol("scalang_ipsf@localhost"), cookie)
      node.ping(Symbol("taco_truck@localhost"), pingTimeout) must ==(false)
    }

    "send local regname" in {
      node = Node(Symbol("scalang_slr@localhost"), cookie)
      val regName = Symbol("echo_slr")
      val echoPid = node.spawn[EchoProcess](regName)
      val mbox = node.spawnMbox
      Poller.waitFor(() => node.getNames.contains(regName)) must beTrue
      val msg = Symbol("blah_slr")
      node.send(regName, (mbox.self, msg))
      mbox.receive(receiveTimeout) must ==(Some(msg))
    }

    "send remote regname" in {
      node = Node(Symbol("scalang_srr@localhost"), cookie)
      // start `echo` service on node `echo@localhost`
      erl = Escript("echo.escript")
      ReadLine(erl) must ==("ok")
      val echo_host = Symbol("echo@localhost")
      Poller.waitFor(() => node.ping(echo_host, pingTimeout)) must beTrue
      val mbox = node.spawnMbox
      Poller.waitFor(() => node.processes.contains(mbox.self))
      Poller.waitFor(() => {
        val msg = Symbol("blah_srr_" + RandStr(4))
        node.send(('echo, echo_host), mbox.self, (mbox.self, msg))
        mbox.receive(receiveTimeout) == (Some(msg))
      }) must beTrue
    }

    "receive remove regname" in {
      val nodeName = Symbol("scalang_rrr@localhost")
      node = Node(nodeName, cookie)
      // start `echo` service on node `echo@localhost`
      erl = Escript("echo.escript")
      ReadLine(erl) must ==("ok")
      val echo_host = Symbol("echo@localhost")
      Poller.waitFor(() => node.ping(echo_host, pingTimeout)) must beTrue
      val mboxName = Symbol("mbox_rrr")
      val mbox = node.spawnMbox(mboxName)
      Poller.waitFor(() => node.getNames.contains(mboxName)) must beTrue
      val msg = Symbol("blah_rrr")
      Poller.waitFor(() => {
        node.send(('echo, echo_host), mbox.self, ((mboxName, nodeName), msg))
        mbox.receive(receiveTimeout) == Some(msg)
      }) must beTrue
    }

    "remove processes on exit" in {
      node = Node(Symbol("scalang_rpoe@localhost"), cookie)
      val pid = node.spawn[FailProcess]
      node.processes.get(pid) must beLike { case f : ProcessLauncher[_] => true }
      node.handleSend(pid, 'bah)
      Poller.waitFor(() => node.processes.get(pid) == null) must beTrue
    }

    "deliver local breakages" in {
      node = Node(Symbol("scalang_dlb@localhost"), cookie)
      val linkProc = node.spawn[LinkProcess]
      val failProc = node.spawn[FailProcess]
      val mbox = node.spawnMbox
      node.send(linkProc, (failProc, mbox.self))
      mbox.receive(receiveTimeout) must ==(Some('ok))
      node.send(failProc, 'fail)
      Poller.waitFor(() =>
        !node.isAlive(failProc) && !node.isAlive(linkProc)
      ) must beTrue
    }

    "deliver remote breakages" in {
      node = Node(Symbol("scala_break@localhost"), cookie)
      val mbox = node.spawnMbox('mbox_break)
      val scala = node.spawnMbox('scala_break)
      Poller.waitFor(() => {
        erl = Escript("link_delivery.escript")
        mbox.receive(receiveTimeout) match {
          case Some(msg) => {
            val remotePid = msg.asInstanceOf[Pid]
            mbox.link(remotePid)
            mbox.exit('blah_break)
            scala.receive(receiveTimeout) == Some('blah_break)
          }
          case None => false
        }
      }) must beTrue
    }

    "deliver local breakages" in {
      node = Node(Symbol("scala_break@localhost"), cookie)
      val mbox = node.spawnMbox('mbox_break)
      erl = Escript("link_delivery.escript")
      val remotePid = mbox.receive.asInstanceOf[Pid]
      mbox.link(remotePid)
      node.send(remotePid, 'blah_break)
      Thread.sleep(200)
      node.isAlive(mbox.self) must ==(false)
    }

    "deliver breaks on channel disconnect" in {
       println("discon")
       node = Node(Symbol("scala_break@localhost"), cookie)
       val mbox = node.spawnMbox('mbox_break)
       erl = Escript("link_delivery.escript")
       val remotePid = mbox.receive.asInstanceOf[Pid]
       mbox.link(remotePid)
       erl.destroy
       erl.waitFor
       Thread.sleep(100)
       node.isAlive(mbox.self) must ==(false)
     }

     "deliver local monitor exits" in {
       node = Node(Symbol("scala_dlme@localhost"), cookie)
       val monitorProc = node.spawn[MonitorProcess]
       val failProc = node.spawn[FailProcess]
       val mbox = node.spawnMbox
       node.send(monitorProc, (failProc, mbox.self))
       Thread.sleep(100)
       mbox.receive must ==('ok)
       node.send(failProc, 'fail)
       Thread.sleep(100)
       mbox.receive must ==('monitor_exit)
       node.isAlive(failProc) must ==(false)
       node.isAlive(monitorProc) must ==(true)
     }

     "deliver remote monitor exits" in {
       node = Node(Symbol("scala_monitor@localhost"), cookie)
       val mbox = node.spawnMbox('mbox_monitor)
       val scala = node.spawnMbox('scala_monitor)
       erl = Escript("monitor.escript")
       val remotePid = mbox.receive.asInstanceOf[Pid]

       // tell remote node to monitor our mbox.
       node.send(remotePid, ('monitor, mbox.self))
       val remoteRef = scala.receive.asInstanceOf[Reference]

       // kill our mbox and await notification from remote node.
       mbox.exit('blah_monitor)
       scala.receive must ==(('down, 'blah_monitor))
     }

     "don't deliver remote monitor exit after demonitor" in {
       node = Node(Symbol("scala_monitor@localhost"), cookie)
       val mbox = node.spawnMbox('mbox_monitor)
       val scala = node.spawnMbox('scala_monitor)
       erl = Escript("monitor.escript")
       val remotePid = mbox.receive.asInstanceOf[Pid]

       // tell remote node to monitor our mbox.
       node.send(remotePid, ('monitor, mbox.self))
       val remoteRef = scala.receive.asInstanceOf[Reference]

       // tell remote node to stop monitoring our mbox.
       node.send(remotePid, ('demonitor, remoteRef))
       scala.receive must ==(('demonitor, remoteRef))

       // kill our mbox and expect no notification from remote node.
       mbox.exit('blah)
       scala.receive(100) must ==(None)
     }

     "receive remote monitor exits" in {
       node = Node(Symbol("scala_monitor@localhost"), cookie)
       val monitorProc = node.spawn[MonitorProcess]
       val mbox = node.spawnMbox('mbox_monitor)
       val scala = node.spawn[MonitorProcess]('scala_monitor)
       erl = Escript("monitor.escript")
       val remotePid = mbox.receive.asInstanceOf[Pid]

       node.send(monitorProc, (remotePid, mbox.self))
       Thread.sleep(100)
       mbox.receive must ==('ok)
       node.send(monitorProc, ('exit, 'blah))
       Thread.sleep(100)
       mbox.receive must ==('monitor_exit)
       node.isAlive(monitorProc) must ==(true)
     }

     "deliver local monitor exit for unregistered process" in {
       node = Node(Symbol("scala_monitor@localhost"), cookie)
       val mbox = node.spawnMbox
       val ref = mbox.monitor('foo)
       Thread.sleep(100)
       mbox.receive must ==('DOWN, ref, 'process, 'foo, 'noproc)
     }

     "deliver remote monitor exit for unregistered process" in {
       node = Node(Symbol("scala_monitor@localhost"), cookie)
       val mbox = node.spawnMbox('mbox_monitor)
       val scala = node.spawnMbox('scala_monitor)
       erl = Escript("monitor.escript")
       val remotePid = mbox.receive.asInstanceOf[Pid]
       node.send(remotePid, ('monitor, 'foo))
       val remoteRef = scala.receive.asInstanceOf[Reference]
       scala.receive must ==(('down, 'noproc))
     }

  }
}
