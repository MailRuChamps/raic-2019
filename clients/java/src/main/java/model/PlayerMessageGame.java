package model;

import util.StreamUtil;

public abstract class PlayerMessageGame {
    public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;
    public static PlayerMessageGame readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
            case CustomDataMessage.TAG:
                return CustomDataMessage.readFrom(stream);
            case ActionMessage.TAG:
                return ActionMessage.readFrom(stream);
            default:
                throw new java.io.IOException("Unexpected discriminant value");
        }
    }

    public static class CustomDataMessage extends PlayerMessageGame {
        public static final int TAG = 0;
        private model.CustomData data;
        public model.CustomData getData() { return data; }
        public void setData(model.CustomData data) { this.data = data; }
        public CustomDataMessage() {}
        public CustomDataMessage(model.CustomData data) {
            this.data = data;
        }
        public static CustomDataMessage readFrom(java.io.InputStream stream) throws java.io.IOException {
            CustomDataMessage result = new CustomDataMessage();
            result.data = model.CustomData.readFrom(stream);
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            data.writeTo(stream);
        }
    }

    public static class ActionMessage extends PlayerMessageGame {
        public static final int TAG = 1;
        private java.util.Map<Integer, model.UnitAction> action;
        public java.util.Map<Integer, model.UnitAction> getAction() { return action; }
        public void setAction(java.util.Map<Integer, model.UnitAction> action) { this.action = action; }
        public ActionMessage() {}
        public ActionMessage(java.util.Map<Integer, model.UnitAction> action) {
            this.action = action;
        }
        public static ActionMessage readFrom(java.io.InputStream stream) throws java.io.IOException {
            ActionMessage result = new ActionMessage();
            int actionSize = StreamUtil.readInt(stream);
            result.action = new java.util.HashMap<>(actionSize);
            for (int i = 0; i < actionSize; i++) {
                int actionKey;
                actionKey = StreamUtil.readInt(stream);
                model.UnitAction actionValue;
                actionValue = model.UnitAction.readFrom(stream);
                result.action.put(actionKey, actionValue);
            }
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, action.size());
            for (java.util.Map.Entry<Integer, model.UnitAction> actionEntry : action.entrySet()) {
                int actionKey = actionEntry.getKey();
                model.UnitAction actionValue = actionEntry.getValue();
                StreamUtil.writeInt(stream, actionKey);
                actionValue.writeTo(stream);
            }
        }
    }
}
