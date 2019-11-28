namespace AiCup2019.Model
{
    public struct Weapon
    {
        public Model.WeaponType Typ { get; set; }
        public Model.WeaponParameters Parameters { get; set; }
        public int Magazine { get; set; }
        public bool WasShooting { get; set; }
        public double Spread { get; set; }
        public double? FireTimer { get; set; }
        public double? LastAngle { get; set; }
        public int? LastFireTick { get; set; }
        public Weapon(Model.WeaponType typ, Model.WeaponParameters parameters, int magazine, bool wasShooting, double spread, double? fireTimer, double? lastAngle, int? lastFireTick)
        {
            this.Typ = typ;
            this.Parameters = parameters;
            this.Magazine = magazine;
            this.WasShooting = wasShooting;
            this.Spread = spread;
            this.FireTimer = fireTimer;
            this.LastAngle = lastAngle;
            this.LastFireTick = lastFireTick;
        }
        public static Weapon ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Weapon();
            switch (reader.ReadInt32())
            {
            case 0:
                result.Typ = Model.WeaponType.Pistol;
                break;
            case 1:
                result.Typ = Model.WeaponType.AssaultRifle;
                break;
            case 2:
                result.Typ = Model.WeaponType.RocketLauncher;
                break;
            default:
                throw new System.Exception("Unexpected discriminant value");
            }
            result.Parameters = Model.WeaponParameters.ReadFrom(reader);
            result.Magazine = reader.ReadInt32();
            result.WasShooting = reader.ReadBoolean();
            result.Spread = reader.ReadDouble();
            if (reader.ReadBoolean())
            {
                result.FireTimer = reader.ReadDouble();
            } else
            {
                result.FireTimer = null;
            }
            if (reader.ReadBoolean())
            {
                result.LastAngle = reader.ReadDouble();
            } else
            {
                result.LastAngle = null;
            }
            if (reader.ReadBoolean())
            {
                result.LastFireTick = reader.ReadInt32();
            } else
            {
                result.LastFireTick = null;
            }
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write((int) (Typ));
            Parameters.WriteTo(writer);
            writer.Write(Magazine);
            writer.Write(WasShooting);
            writer.Write(Spread);
            if (!FireTimer.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(FireTimer.Value);
            }
            if (!LastAngle.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(LastAngle.Value);
            }
            if (!LastFireTick.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(LastFireTick.Value);
            }
        }
    }
}
