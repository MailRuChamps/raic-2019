import java.io.IOException;
import java.io.OutputStream;

public class Debug {
    private OutputStream stream;

    public Debug(OutputStream stream) {
        this.stream = stream;
    }

    public void draw(model.CustomData customData) {
        try {
            new model.PlayerMessageGame.CustomDataMessage(customData).writeTo(stream);
            stream.flush();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}