import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Game {
    int currentTick;
    Properties properties;
    Level level;
    Player[] players;
    Unit[] units;
    Bullet[] bullets;
    Mine[] mines;
    LootBox[] lootBoxes;
    this(int currentTick, Properties properties, Level level, Player[] players, Unit[] units, Bullet[] bullets, Mine[] mines, LootBox[] lootBoxes) {
        this.currentTick = currentTick;
        this.properties = properties;
        this.level = level;
        this.players = players;
        this.units = units;
        this.bullets = bullets;
        this.mines = mines;
        this.lootBoxes = lootBoxes;
    }
    static Game readFrom(Stream reader) {
        auto result = Game();
        result.currentTick = reader.readInt();
        result.properties = Properties.readFrom(reader);
        result.level = Level.readFrom(reader);
        result.players = new Player[reader.readInt()];
        for (int i = 0; i < result.players.length; i++) {
            result.players[i] = Player.readFrom(reader);
        }
        result.units = new Unit[reader.readInt()];
        for (int i = 0; i < result.units.length; i++) {
            result.units[i] = Unit.readFrom(reader);
        }
        result.bullets = new Bullet[reader.readInt()];
        for (int i = 0; i < result.bullets.length; i++) {
            result.bullets[i] = Bullet.readFrom(reader);
        }
        result.mines = new Mine[reader.readInt()];
        for (int i = 0; i < result.mines.length; i++) {
            result.mines[i] = Mine.readFrom(reader);
        }
        result.lootBoxes = new LootBox[reader.readInt()];
        for (int i = 0; i < result.lootBoxes.length; i++) {
            result.lootBoxes[i] = LootBox.readFrom(reader);
        }
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(currentTick);
        properties.writeTo(writer);
        level.writeTo(writer);
        writer.write(cast(int)(players.length));
        foreach (playersElement; players) {
            playersElement.writeTo(writer);
        }
        writer.write(cast(int)(units.length));
        foreach (unitsElement; units) {
            unitsElement.writeTo(writer);
        }
        writer.write(cast(int)(bullets.length));
        foreach (bulletsElement; bullets) {
            bulletsElement.writeTo(writer);
        }
        writer.write(cast(int)(mines.length));
        foreach (minesElement; mines) {
            minesElement.writeTo(writer);
        }
        writer.write(cast(int)(lootBoxes.length));
        foreach (lootBoxesElement; lootBoxes) {
            lootBoxesElement.writeTo(writer);
        }
    }
    string toString() const {
        return "Game" ~ "(" ~
            to!string(currentTick) ~
            to!string(properties) ~
            to!string(level) ~
            to!string(players) ~
            to!string(units) ~
            to!string(bullets) ~
            to!string(mines) ~
            to!string(lootBoxes) ~
            ")";
    }
}
