namespace AiCup2019.Model
{
    public struct Mine
    {
        public int PlayerId { get; set; }
        public Model.Vec2Double Position { get; set; }
        public Model.Vec2Double Size { get; set; }
        public Model.MineState State { get; set; }
        public double? Timer { get; set; }
        public double TriggerRadius { get; set; }
        public Model.ExplosionParameters ExplosionParameters { get; set; }
        public Mine(int playerId, Model.Vec2Double position, Model.Vec2Double size, Model.MineState state, double? timer, double triggerRadius, Model.ExplosionParameters explosionParameters)
        {
            this.PlayerId = playerId;
            this.Position = position;
            this.Size = size;
            this.State = state;
            this.Timer = timer;
            this.TriggerRadius = triggerRadius;
            this.ExplosionParameters = explosionParameters;
        }
        public static Mine ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Mine();
            result.PlayerId = reader.ReadInt32();
            result.Position = Model.Vec2Double.ReadFrom(reader);
            result.Size = Model.Vec2Double.ReadFrom(reader);
            switch (reader.ReadInt32())
            {
            case 0:
                result.State = Model.MineState.Preparing;
                break;
            case 1:
                result.State = Model.MineState.Idle;
                break;
            case 2:
                result.State = Model.MineState.Triggered;
                break;
            case 3:
                result.State = Model.MineState.Exploded;
                break;
            default:
                throw new System.Exception("Unexpected discriminant value");
            }
            if (reader.ReadBoolean())
            {
                result.Timer = reader.ReadDouble();
            } else
            {
                result.Timer = null;
            }
            result.TriggerRadius = reader.ReadDouble();
            result.ExplosionParameters = Model.ExplosionParameters.ReadFrom(reader);
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(PlayerId);
            Position.WriteTo(writer);
            Size.WriteTo(writer);
            writer.Write((int) (State));
            if (!Timer.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(Timer.Value);
            }
            writer.Write(TriggerRadius);
            ExplosionParameters.WriteTo(writer);
        }
    }
}
