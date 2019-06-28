package pubsub.command

import pubsub.Client
import pubsub.collection._

class CommandHandler(buffer: BoundedBuffer[Command]) {
  import CommandHandler._
  
  def handle(): Unit = {
    val command = buffer.take()

    command match {
      case Subscribe(topic, client) =>
        multiMap.add(topic, client)
        client.sendAck("subscribe", topic)

      case Unsubscribe(topic, client) =>
        multiMap.remove(topic, client)
        client.sendAck("unsubscribe", topic)

      case Publish(topic, message, sender) =>
        for {
          subscribers <- multiMap.get(topic)
          client <- subscribers
        } client.sendMessage(sender.name, topic, message)

      case EndOfClient(client) =>
        multiMap.removeValueFromAll(client)

      case Rename(newName,client) =>
        client.name = newName
        client.sendAck("rename", newName)

      case _ =>
        // nothing should happen
    }
  }
}


object CommandHandler {
  val multiMap = new ConcurrentMultiMap[String, Client]()
}
