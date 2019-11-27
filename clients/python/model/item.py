class Item:
    @staticmethod
    def read_from(stream):
        discriminant = stream.read_int()
        if discriminant == HealthPack.TAG:
            return Item.HealthPack.read_from(stream)
        if discriminant == Weapon.TAG:
            return Item.Weapon.read_from(stream)
        if discriminant == Mine.TAG:
            return Item.Mine.read_from(stream)
        raise Exception("Unexpected discriminant value")

class HealthPack(Item):
    TAG = 0
    def __init__(self, health):
        self.health = health
    @staticmethod
    def read_from(stream):
        health = stream.read_int()
        return HealthPack(health)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_int(self.health)
    def __repr__(self):
        return "HealthPack(" + \
            repr(self.health) + \
            ")"
Item.HealthPack = HealthPack
from .weapon_type import WeaponType
class Weapon(Item):
    TAG = 1
    def __init__(self, weapon_type):
        self.weapon_type = weapon_type
    @staticmethod
    def read_from(stream):
        weapon_type = WeaponType(stream.read_int())
        return Weapon(weapon_type)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_int(self.weapon_type)
    def __repr__(self):
        return "Weapon(" + \
            repr(self.weapon_type) + \
            ")"
Item.Weapon = Weapon
class Mine(Item):
    TAG = 2
    def __init__(self):
        pass
    @staticmethod
    def read_from(stream):
        return Mine()
    def write_to(self, stream):
        stream.write_int(self.TAG)
    def __repr__(self):
        return "Mine(" + \
            ")"
Item.Mine = Mine
