package model;

import util.StreamUtil;

public enum TextAlignment {
    LEFT(0),
    CENTER(1),
    RIGHT(2);
    public int discriminant;
    TextAlignment(int discriminant) {
      this.discriminant = discriminant;
    }
}
