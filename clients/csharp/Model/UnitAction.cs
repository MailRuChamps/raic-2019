namespace AiCup2019.Model
{
    public struct UnitAction
    {
        public double Velocity { get; set; }
        public bool Jump { get; set; }
        public bool JumpDown { get; set; }
        public Model.Vec2Double Aim { get; set; }
        public bool Shoot { get; set; }
        public bool SwapWeapon { get; set; }
        public bool PlantMine { get; set; }
        public UnitAction(double velocity, bool jump, bool jumpDown, Model.Vec2Double aim, bool shoot, bool swapWeapon, bool plantMine)
        {
            this.Velocity = velocity;
            this.Jump = jump;
            this.JumpDown = jumpDown;
            this.Aim = aim;
            this.Shoot = shoot;
            this.SwapWeapon = swapWeapon;
            this.PlantMine = plantMine;
        }
        public static UnitAction ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new UnitAction();
            result.Velocity = reader.ReadDouble();
            result.Jump = reader.ReadBoolean();
            result.JumpDown = reader.ReadBoolean();
            result.Aim = Model.Vec2Double.ReadFrom(reader);
            result.Shoot = reader.ReadBoolean();
            result.SwapWeapon = reader.ReadBoolean();
            result.PlantMine = reader.ReadBoolean();
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(Velocity);
            writer.Write(Jump);
            writer.Write(JumpDown);
            Aim.WriteTo(writer);
            writer.Write(Shoot);
            writer.Write(SwapWeapon);
            writer.Write(PlantMine);
        }
    }
}
