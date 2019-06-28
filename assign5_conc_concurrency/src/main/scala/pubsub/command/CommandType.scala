package pubsub.command

import pubsub.Client

sealed trait Topic {
   val topic: String
}

sealed abstract class Command(from: Client) {
  def toString(): String
}
case class EndOfClient(from: Client) extends Command(from) {
  override def toString(): String = s"${from.name}: End of client"
}
case class MalformedCommand(from: Client) extends Command(from) {
  override def toString(): String = s"${from.name}: Invalid Command"
}
case class Subscribe(topic: String,
    from: Client) extends Command(from) with Topic {
  override def toString(): String = s"${from.name}: Subscribe @ $topic"
}
case class Unsubscribe(topic: String,
    from: Client) extends Command(from) with Topic {
  override def toString(): String = s"${from.name}: Unsubscribe @ $topic"
}
case class Publish(topic: String, message: String,
    from: Client) extends Command(from) with Topic {
  override def toString(): String = s"${from.name}: Publish @ $topic -> $message"
}

case class Rename(newName: String, from: Client) extends Command(from) {
  override def toString(): String = s"${from.name}: Renamed to $newName"
}
