import model;
import stream;

class Debugger {
    this(Stream stream) {
        this.stream = stream;
    }
    void draw(const CustomData data) {
        stream.write(PlayerMessageGame.CustomDataMessage.TAG);
        data.writeTo(stream);
    }
private:
    Stream stream;
}