package scalang.node

import org.specs._
import org.specs.runner._
import scalang.util._
import org.jboss.{netty => netty}
import netty.buffer._
import netty.channel._
import ChannelBuffers._
import java.security.MessageDigest
import netty.handler.codec.embedder.TwoWayCodecEmbedder

class ServerHandshakeHandlerSpec extends SpecificationWithJUnit {
  val cookie = "DRSJLFJLGIYPEAVFYFCY"
  val creation = 1

  "ServerHandshakeHandler" should {
    "complete a standard handshake" in {
      val handshake = new ServerHandshakeHandler(Symbol("tmp@blah"), cookie, creation, { (peer : Symbol, p : ChannelPipeline) =>

      })
      val embedder = new TwoWayCodecEmbedder[Any](handshake)
      embedder.upstreamMessage(NameMessage(32765, creation, "tmp@moonpolysoft.local"))
      val status = embedder.poll
      status must ==(StatusMessage("ok"))
      var challenge = 0
      val challengeMsg = embedder.poll
      challengeMsg must beLike { case ChallengeMessage(_, c : Int, _, _) =>
        challenge = c
        true }
      val md5 = MessageDigest.getInstance("MD5")
      md5.update(cookie.getBytes)
      md5.update(handshake.mask(challenge).toString.getBytes)
      val digest = md5.digest
      //we reuse the same challenge to make the test easier
      embedder.upstreamMessage(ChallengeReplyMessage(challenge, digest))
      val ackMsg = embedder.poll
      ackMsg must beLike { case ChallengeAckMessage(d) =>
        d.deep == digest.deep
      }
      handshake.isVerified must ==(true)
    }
  }
}
