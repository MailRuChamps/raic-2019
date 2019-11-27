package model;

import util.StreamUtil;

public class Game {
    private int currentTick;
    public int getCurrentTick() { return currentTick; }
    public void setCurrentTick(int currentTick) { this.currentTick = currentTick; }
    private model.Properties properties;
    public model.Properties getProperties() { return properties; }
    public void setProperties(model.Properties properties) { this.properties = properties; }
    private model.Level level;
    public model.Level getLevel() { return level; }
    public void setLevel(model.Level level) { this.level = level; }
    private model.Player[] players;
    public model.Player[] getPlayers() { return players; }
    public void setPlayers(model.Player[] players) { this.players = players; }
    private model.Unit[] units;
    public model.Unit[] getUnits() { return units; }
    public void setUnits(model.Unit[] units) { this.units = units; }
    private model.Bullet[] bullets;
    public model.Bullet[] getBullets() { return bullets; }
    public void setBullets(model.Bullet[] bullets) { this.bullets = bullets; }
    private model.Mine[] mines;
    public model.Mine[] getMines() { return mines; }
    public void setMines(model.Mine[] mines) { this.mines = mines; }
    private model.LootBox[] lootBoxes;
    public model.LootBox[] getLootBoxes() { return lootBoxes; }
    public void setLootBoxes(model.LootBox[] lootBoxes) { this.lootBoxes = lootBoxes; }
    public Game() {}
    public Game(int currentTick, model.Properties properties, model.Level level, model.Player[] players, model.Unit[] units, model.Bullet[] bullets, model.Mine[] mines, model.LootBox[] lootBoxes) {
        this.currentTick = currentTick;
        this.properties = properties;
        this.level = level;
        this.players = players;
        this.units = units;
        this.bullets = bullets;
        this.mines = mines;
        this.lootBoxes = lootBoxes;
    }
    public static Game readFrom(java.io.InputStream stream) throws java.io.IOException {
        Game result = new Game();
        result.currentTick = StreamUtil.readInt(stream);
        result.properties = model.Properties.readFrom(stream);
        result.level = model.Level.readFrom(stream);
        result.players = new model.Player[StreamUtil.readInt(stream)];
        for (int i = 0; i < result.players.length; i++) {
            result.players[i] = model.Player.readFrom(stream);
        }
        result.units = new model.Unit[StreamUtil.readInt(stream)];
        for (int i = 0; i < result.units.length; i++) {
            result.units[i] = model.Unit.readFrom(stream);
        }
        result.bullets = new model.Bullet[StreamUtil.readInt(stream)];
        for (int i = 0; i < result.bullets.length; i++) {
            result.bullets[i] = model.Bullet.readFrom(stream);
        }
        result.mines = new model.Mine[StreamUtil.readInt(stream)];
        for (int i = 0; i < result.mines.length; i++) {
            result.mines[i] = model.Mine.readFrom(stream);
        }
        result.lootBoxes = new model.LootBox[StreamUtil.readInt(stream)];
        for (int i = 0; i < result.lootBoxes.length; i++) {
            result.lootBoxes[i] = model.LootBox.readFrom(stream);
        }
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, currentTick);
        properties.writeTo(stream);
        level.writeTo(stream);
        StreamUtil.writeInt(stream, players.length);
        for (model.Player playersElement : players) {
            playersElement.writeTo(stream);
        }
        StreamUtil.writeInt(stream, units.length);
        for (model.Unit unitsElement : units) {
            unitsElement.writeTo(stream);
        }
        StreamUtil.writeInt(stream, bullets.length);
        for (model.Bullet bulletsElement : bullets) {
            bulletsElement.writeTo(stream);
        }
        StreamUtil.writeInt(stream, mines.length);
        for (model.Mine minesElement : mines) {
            minesElement.writeTo(stream);
        }
        StreamUtil.writeInt(stream, lootBoxes.length);
        for (model.LootBox lootBoxesElement : lootBoxes) {
            lootBoxesElement.writeTo(stream);
        }
    }
}
