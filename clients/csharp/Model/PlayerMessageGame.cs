namespace AiCup2019.Model
{
    public abstract class PlayerMessageGame
    {
        public abstract void WriteTo(System.IO.BinaryWriter writer);
        public static PlayerMessageGame ReadFrom(System.IO.BinaryReader reader)
        {
            switch (reader.ReadInt32())
            {
                case CustomDataMessage.TAG:
                    return CustomDataMessage.ReadFrom(reader);
                case ActionMessage.TAG:
                    return ActionMessage.ReadFrom(reader);
                default:
                    throw new System.Exception("Unexpected discriminant value");
            }
        }

        public class CustomDataMessage : PlayerMessageGame
        {
            public const int TAG = 0;
            public Model.CustomData Data { get; set; }
            public CustomDataMessage() {}
            public CustomDataMessage(Model.CustomData data)
            {
                this.Data = data;
            }
            public static new CustomDataMessage ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new CustomDataMessage();
                result.Data = Model.CustomData.ReadFrom(reader);
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                Data.WriteTo(writer);
            }
        }

        public class ActionMessage : PlayerMessageGame
        {
            public const int TAG = 1;
            public System.Collections.Generic.IDictionary<int, Model.UnitAction> Action { get; set; }
            public ActionMessage() {}
            public ActionMessage(System.Collections.Generic.IDictionary<int, Model.UnitAction> action)
            {
                this.Action = action;
            }
            public static new ActionMessage ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new ActionMessage();
                int ActionSize = reader.ReadInt32();
                result.Action = new System.Collections.Generic.Dictionary<int, Model.UnitAction>(ActionSize);
                for (int i = 0; i < ActionSize; i++)
                {
                    int ActionKey;
                    ActionKey = reader.ReadInt32();
                    Model.UnitAction ActionValue;
                    ActionValue = Model.UnitAction.ReadFrom(reader);
                    result.Action.Add(ActionKey, ActionValue);
                }
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(Action.Count);
                foreach (var ActionEntry in Action)
                {
                    var ActionKey = ActionEntry.Key;
                    var ActionValue = ActionEntry.Value;
                    writer.Write(ActionKey);
                    ActionValue.WriteTo(writer);
                }
            }
        }
    }
}
