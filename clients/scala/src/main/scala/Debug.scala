import java.io.OutputStream

class Debug(private val stream: OutputStream) {
  def draw(customData: model.CustomData) {
    model.PlayerMessageGame.CustomDataMessage(customData).writeTo(stream)
    stream.flush()
  }
}