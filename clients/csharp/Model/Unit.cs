namespace AiCup2019.Model
{
    public struct Unit
    {
        public int PlayerId { get; set; }
        public int Id { get; set; }
        public int Health { get; set; }
        public Model.Vec2Double Position { get; set; }
        public Model.Vec2Double Size { get; set; }
        public Model.JumpState JumpState { get; set; }
        public bool WalkedRight { get; set; }
        public bool Stand { get; set; }
        public bool OnGround { get; set; }
        public bool OnLadder { get; set; }
        public int Mines { get; set; }
        public Model.Weapon? Weapon { get; set; }
        public Unit(int playerId, int id, int health, Model.Vec2Double position, Model.Vec2Double size, Model.JumpState jumpState, bool walkedRight, bool stand, bool onGround, bool onLadder, int mines, Model.Weapon? weapon)
        {
            this.PlayerId = playerId;
            this.Id = id;
            this.Health = health;
            this.Position = position;
            this.Size = size;
            this.JumpState = jumpState;
            this.WalkedRight = walkedRight;
            this.Stand = stand;
            this.OnGround = onGround;
            this.OnLadder = onLadder;
            this.Mines = mines;
            this.Weapon = weapon;
        }
        public static Unit ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Unit();
            result.PlayerId = reader.ReadInt32();
            result.Id = reader.ReadInt32();
            result.Health = reader.ReadInt32();
            result.Position = Model.Vec2Double.ReadFrom(reader);
            result.Size = Model.Vec2Double.ReadFrom(reader);
            result.JumpState = Model.JumpState.ReadFrom(reader);
            result.WalkedRight = reader.ReadBoolean();
            result.Stand = reader.ReadBoolean();
            result.OnGround = reader.ReadBoolean();
            result.OnLadder = reader.ReadBoolean();
            result.Mines = reader.ReadInt32();
            if (reader.ReadBoolean())
            {
                result.Weapon = Model.Weapon.ReadFrom(reader);
            } else
            {
                result.Weapon = null;
            }
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(PlayerId);
            writer.Write(Id);
            writer.Write(Health);
            Position.WriteTo(writer);
            Size.WriteTo(writer);
            JumpState.WriteTo(writer);
            writer.Write(WalkedRight);
            writer.Write(Stand);
            writer.Write(OnGround);
            writer.Write(OnLadder);
            writer.Write(Mines);
            if (!Weapon.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                Weapon.Value.WriteTo(writer);
            }
        }
    }
}
