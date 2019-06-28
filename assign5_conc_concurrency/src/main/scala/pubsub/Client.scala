package pubsub

import java.net.Socket
import java.nio.charset.Charset

case class Client(socket: Socket, id: Int)
    (implicit charset: Charset = Charset.forName("UTF-8")) {
  private var name_ = "client_" + id
  val outStream = socket.getOutputStream()

  def name = name_
  def name_=(newName: String) = name_ = newName 

  def isConnected: Boolean = socket.isConnected()

  def close(): Unit = socket.close()

  def send(message: String): Unit = {
    val payload = message.getBytes(charset)
    outStream.write(payload)
    outStream.flush()
  }

  def sendAck(ackType: String, message: String): Unit =
    send(s"${ackType}_ack $message\n")

  def sayHello(): Unit = sendAck("connection", name_)
  def sayGoodbye(): Unit = send(s"Bye Bye dear $name_!\n")

  def invalidPreviousCommand(): Unit = send("! previous command was invalid\n")

  def sendMessage(sender: String, topic: String, message: String): Unit =
    send(s"$sender@$topic $message\n")
}