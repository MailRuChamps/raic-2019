package model;

import util.StreamUtil;

public enum WeaponType {
    PISTOL(0),
    ASSAULT_RIFLE(1),
    ROCKET_LAUNCHER(2);
    public int discriminant;
    WeaponType(int discriminant) {
      this.discriminant = discriminant;
    }
}
