namespace AiCup2019.Model
{
    public abstract class Item
    {
        public abstract void WriteTo(System.IO.BinaryWriter writer);
        public static Item ReadFrom(System.IO.BinaryReader reader)
        {
            switch (reader.ReadInt32())
            {
                case HealthPack.TAG:
                    return HealthPack.ReadFrom(reader);
                case Weapon.TAG:
                    return Weapon.ReadFrom(reader);
                case Mine.TAG:
                    return Mine.ReadFrom(reader);
                default:
                    throw new System.Exception("Unexpected discriminant value");
            }
        }

        public class HealthPack : Item
        {
            public const int TAG = 0;
            public int Health { get; set; }
            public HealthPack() {}
            public HealthPack(int health)
            {
                this.Health = health;
            }
            public static new HealthPack ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new HealthPack();
                result.Health = reader.ReadInt32();
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(Health);
            }
        }

        public class Weapon : Item
        {
            public const int TAG = 1;
            public Model.WeaponType WeaponType { get; set; }
            public Weapon() {}
            public Weapon(Model.WeaponType weaponType)
            {
                this.WeaponType = weaponType;
            }
            public static new Weapon ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Weapon();
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
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write((int) (WeaponType));
            }
        }

        public class Mine : Item
        {
            public const int TAG = 2;
            public Mine() {}
            public static new Mine ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Mine();
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
            }
        }
    }
}
