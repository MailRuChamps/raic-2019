namespace AiCup2019.Model
{
    public struct Properties
    {
        public int MaxTickCount { get; set; }
        public int TeamSize { get; set; }
        public double TicksPerSecond { get; set; }
        public int UpdatesPerTick { get; set; }
        public Model.Vec2Double LootBoxSize { get; set; }
        public Model.Vec2Double UnitSize { get; set; }
        public double UnitMaxHorizontalSpeed { get; set; }
        public double UnitFallSpeed { get; set; }
        public double UnitJumpTime { get; set; }
        public double UnitJumpSpeed { get; set; }
        public double JumpPadJumpTime { get; set; }
        public double JumpPadJumpSpeed { get; set; }
        public int UnitMaxHealth { get; set; }
        public int HealthPackHealth { get; set; }
        public System.Collections.Generic.IDictionary<Model.WeaponType, Model.WeaponParameters> WeaponParameters { get; set; }
        public Model.Vec2Double MineSize { get; set; }
        public Model.ExplosionParameters MineExplosionParameters { get; set; }
        public double MinePrepareTime { get; set; }
        public double MineTriggerTime { get; set; }
        public double MineTriggerRadius { get; set; }
        public int KillScore { get; set; }
        public Properties(int maxTickCount, int teamSize, double ticksPerSecond, int updatesPerTick, Model.Vec2Double lootBoxSize, Model.Vec2Double unitSize, double unitMaxHorizontalSpeed, double unitFallSpeed, double unitJumpTime, double unitJumpSpeed, double jumpPadJumpTime, double jumpPadJumpSpeed, int unitMaxHealth, int healthPackHealth, System.Collections.Generic.IDictionary<Model.WeaponType, Model.WeaponParameters> weaponParameters, Model.Vec2Double mineSize, Model.ExplosionParameters mineExplosionParameters, double minePrepareTime, double mineTriggerTime, double mineTriggerRadius, int killScore)
        {
            this.MaxTickCount = maxTickCount;
            this.TeamSize = teamSize;
            this.TicksPerSecond = ticksPerSecond;
            this.UpdatesPerTick = updatesPerTick;
            this.LootBoxSize = lootBoxSize;
            this.UnitSize = unitSize;
            this.UnitMaxHorizontalSpeed = unitMaxHorizontalSpeed;
            this.UnitFallSpeed = unitFallSpeed;
            this.UnitJumpTime = unitJumpTime;
            this.UnitJumpSpeed = unitJumpSpeed;
            this.JumpPadJumpTime = jumpPadJumpTime;
            this.JumpPadJumpSpeed = jumpPadJumpSpeed;
            this.UnitMaxHealth = unitMaxHealth;
            this.HealthPackHealth = healthPackHealth;
            this.WeaponParameters = weaponParameters;
            this.MineSize = mineSize;
            this.MineExplosionParameters = mineExplosionParameters;
            this.MinePrepareTime = minePrepareTime;
            this.MineTriggerTime = mineTriggerTime;
            this.MineTriggerRadius = mineTriggerRadius;
            this.KillScore = killScore;
        }
        public static Properties ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Properties();
            result.MaxTickCount = reader.ReadInt32();
            result.TeamSize = reader.ReadInt32();
            result.TicksPerSecond = reader.ReadDouble();
            result.UpdatesPerTick = reader.ReadInt32();
            result.LootBoxSize = Model.Vec2Double.ReadFrom(reader);
            result.UnitSize = Model.Vec2Double.ReadFrom(reader);
            result.UnitMaxHorizontalSpeed = reader.ReadDouble();
            result.UnitFallSpeed = reader.ReadDouble();
            result.UnitJumpTime = reader.ReadDouble();
            result.UnitJumpSpeed = reader.ReadDouble();
            result.JumpPadJumpTime = reader.ReadDouble();
            result.JumpPadJumpSpeed = reader.ReadDouble();
            result.UnitMaxHealth = reader.ReadInt32();
            result.HealthPackHealth = reader.ReadInt32();
            int WeaponParametersSize = reader.ReadInt32();
            result.WeaponParameters = new System.Collections.Generic.Dictionary<Model.WeaponType, Model.WeaponParameters>(WeaponParametersSize);
            for (int i = 0; i < WeaponParametersSize; i++)
            {
                Model.WeaponType WeaponParametersKey;
                switch (reader.ReadInt32())
                {
                case 0:
                    WeaponParametersKey = Model.WeaponType.Pistol;
                    break;
                case 1:
                    WeaponParametersKey = Model.WeaponType.AssaultRifle;
                    break;
                case 2:
                    WeaponParametersKey = Model.WeaponType.RocketLauncher;
                    break;
                default:
                    throw new System.Exception("Unexpected discriminant value");
                }
                Model.WeaponParameters WeaponParametersValue;
                WeaponParametersValue = Model.WeaponParameters.ReadFrom(reader);
                result.WeaponParameters.Add(WeaponParametersKey, WeaponParametersValue);
            }
            result.MineSize = Model.Vec2Double.ReadFrom(reader);
            result.MineExplosionParameters = Model.ExplosionParameters.ReadFrom(reader);
            result.MinePrepareTime = reader.ReadDouble();
            result.MineTriggerTime = reader.ReadDouble();
            result.MineTriggerRadius = reader.ReadDouble();
            result.KillScore = reader.ReadInt32();
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(MaxTickCount);
            writer.Write(TeamSize);
            writer.Write(TicksPerSecond);
            writer.Write(UpdatesPerTick);
            LootBoxSize.WriteTo(writer);
            UnitSize.WriteTo(writer);
            writer.Write(UnitMaxHorizontalSpeed);
            writer.Write(UnitFallSpeed);
            writer.Write(UnitJumpTime);
            writer.Write(UnitJumpSpeed);
            writer.Write(JumpPadJumpTime);
            writer.Write(JumpPadJumpSpeed);
            writer.Write(UnitMaxHealth);
            writer.Write(HealthPackHealth);
            writer.Write(WeaponParameters.Count);
            foreach (var WeaponParametersEntry in WeaponParameters)
            {
                var WeaponParametersKey = WeaponParametersEntry.Key;
                var WeaponParametersValue = WeaponParametersEntry.Value;
                writer.Write((int) (WeaponParametersKey));
                WeaponParametersValue.WriteTo(writer);
            }
            MineSize.WriteTo(writer);
            MineExplosionParameters.WriteTo(writer);
            writer.Write(MinePrepareTime);
            writer.Write(MineTriggerTime);
            writer.Write(MineTriggerRadius);
            writer.Write(KillScore);
        }
    }
}
