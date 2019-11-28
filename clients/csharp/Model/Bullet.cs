namespace AiCup2019.Model
{
    public struct Bullet
    {
        public Model.WeaponType WeaponType { get; set; }
        public int UnitId { get; set; }
        public int PlayerId { get; set; }
        public Model.Vec2Double Position { get; set; }
        public Model.Vec2Double Velocity { get; set; }
        public int Damage { get; set; }
        public double Size { get; set; }
        public Model.ExplosionParameters? ExplosionParameters { get; set; }
        public Bullet(Model.WeaponType weaponType, int unitId, int playerId, Model.Vec2Double position, Model.Vec2Double velocity, int damage, double size, Model.ExplosionParameters? explosionParameters)
        {
            this.WeaponType = weaponType;
            this.UnitId = unitId;
            this.PlayerId = playerId;
            this.Position = position;
            this.Velocity = velocity;
            this.Damage = damage;
            this.Size = size;
            this.ExplosionParameters = explosionParameters;
        }
        public static Bullet ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Bullet();
            switch (reader.ReadInt32())
            {
            case 0:
                result.WeaponType = Model.WeaponType.Pistol;
                break;
            case 1:
                result.WeaponType = Model.WeaponType.AssaultRifle;
                break;
            case 2:
                result.WeaponType = Model.WeaponType.RocketLauncher;
                break;
            default:
                throw new System.Exception("Unexpected discriminant value");
            }
            result.UnitId = reader.ReadInt32();
            result.PlayerId = reader.ReadInt32();
            result.Position = Model.Vec2Double.ReadFrom(reader);
            result.Velocity = Model.Vec2Double.ReadFrom(reader);
            result.Damage = reader.ReadInt32();
            result.Size = reader.ReadDouble();
            if (reader.ReadBoolean())
            {
                result.ExplosionParameters = Model.ExplosionParameters.ReadFrom(reader);
            } else
            {
                result.ExplosionParameters = null;
            }
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write((int) (WeaponType));
            writer.Write(UnitId);
            writer.Write(PlayerId);
            Position.WriteTo(writer);
            Velocity.WriteTo(writer);
            writer.Write(Damage);
            writer.Write(Size);
            if (!ExplosionParameters.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                ExplosionParameters.Value.WriteTo(writer);
            }
        }
    }
}
