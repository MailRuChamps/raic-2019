package model;

import util.StreamUtil;

public class Versioned {
    private java.util.Map<Integer, model.UnitAction> inner;
    public java.util.Map<Integer, model.UnitAction> getInner() { return inner; }
    public void setInner(java.util.Map<Integer, model.UnitAction> inner) { this.inner = inner; }
    public Versioned() {}
    public Versioned(java.util.Map<Integer, model.UnitAction> inner) {
        this.inner = inner;
    }
    public static Versioned readFrom(java.io.InputStream stream) throws java.io.IOException {
        Versioned result = new Versioned();
        int innerSize = StreamUtil.readInt(stream);
        result.inner = new java.util.HashMap<>(innerSize);
        for (int i = 0; i < innerSize; i++) {
            int innerKey;
            innerKey = StreamUtil.readInt(stream);
            model.UnitAction innerValue;
            innerValue = model.UnitAction.readFrom(stream);
            result.inner.put(innerKey, innerValue);
        }
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, 43981);
        StreamUtil.writeInt(stream, inner.size());
        for (java.util.Map.Entry<Integer, model.UnitAction> innerEntry : inner.entrySet()) {
            int innerKey = innerEntry.getKey();
            model.UnitAction innerValue = innerEntry.getValue();
            StreamUtil.writeInt(stream, innerKey);
            innerValue.writeTo(stream);
        }
    }
}
