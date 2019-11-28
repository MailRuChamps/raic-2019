namespace AiCup2019.Model
{
    public struct WeaponParameters
    {
        public int MagazineSize { get; set; }
        public double FireRate { get; set; }
        public double ReloadTime { get; set; }
        public double MinSpread { get; set; }
        public double MaxSpread { get; set; }
        public double Recoil { get; set; }
        public double AimSpeed { get; set; }
        public Model.BulletParameters Bullet { get; set; }
        public Model.ExplosionParameters? Explosion { get; set; }
        public WeaponParameters(int magazineSize, double fireRate, double reloadTime, double minSpread, double maxSpread, double recoil, double aimSpeed, Model.BulletParameters bullet, Model.ExplosionParameters? explosion)
        {
            this.MagazineSize = magazineSize;
            this.FireRate = fireRate;
            this.ReloadTime = reloadTime;
            this.MinSpread = minSpread;
            this.MaxSpread = maxSpread;
            this.Recoil = recoil;
            this.AimSpeed = aimSpeed;
            this.Bullet = bullet;
            this.Explosion = explosion;
        }
        public static WeaponParameters ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new WeaponParameters();
            result.MagazineSize = reader.ReadInt32();
            result.FireRate = reader.ReadDouble();
            result.ReloadTime = reader.ReadDouble();
            result.MinSpread = reader.ReadDouble();
            result.MaxSpread = reader.ReadDouble();
            result.Recoil = reader.ReadDouble();
            result.AimSpeed = reader.ReadDouble();
            result.Bullet = Model.BulletParameters.ReadFrom(reader);
            if (reader.ReadBoolean())
            {
                result.Explosion = Model.ExplosionParameters.ReadFrom(reader);
            } else
            {
                result.Explosion = null;
            }
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(MagazineSize);
            writer.Write(FireRate);
            writer.Write(ReloadTime);
            writer.Write(MinSpread);
            writer.Write(MaxSpread);
            writer.Write(Recoil);
            writer.Write(AimSpeed);
            Bullet.WriteTo(writer);
            if (!Explosion.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                Explosion.Value.WriteTo(writer);
            }
        }
    }
}
