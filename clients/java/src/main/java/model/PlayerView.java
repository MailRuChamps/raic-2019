package model;

import util.StreamUtil;

public class PlayerView {
    private int myId;
    public int getMyId() { return myId; }
    public void setMyId(int myId) { this.myId = myId; }
    private model.Game game;
    public model.Game getGame() { return game; }
    public void setGame(model.Game game) { this.game = game; }
    public PlayerView() {}
    public PlayerView(int myId, model.Game game) {
        this.myId = myId;
        this.game = game;
    }
    public static PlayerView readFrom(java.io.InputStream stream) throws java.io.IOException {
        PlayerView result = new PlayerView();
        result.myId = StreamUtil.readInt(stream);
        result.game = model.Game.readFrom(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, myId);
        game.writeTo(stream);
    }
}
