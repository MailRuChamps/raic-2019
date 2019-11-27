require_relative 'weapon_type'
class Item
    def self.read_from(stream)
        discriminant = stream.read_int()
        if discriminant == Item::HealthPack::TAG
            return Item::HealthPack.read_from(stream)
        end
        if discriminant == Item::Weapon::TAG
            return Item::Weapon.read_from(stream)
        end
        if discriminant == Item::Mine::TAG
            return Item::Mine.read_from(stream)
        end
        raise "Unexpected discriminant value"
    end

    class HealthPack
        TAG = 0
        attr_accessor :health
        def initialize(health)
            @health = health
        end
        def self.read_from(stream)
            health = stream.read_int()
            HealthPack.new(health)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@health)
        end
    end
    class Weapon
        TAG = 1
        attr_accessor :weapon_type
        def initialize(weapon_type)
            @weapon_type = weapon_type
        end
        def self.read_from(stream)
            weapon_type = stream.read_int()
            if weapon_type < 0 || weapon_type > 3
                raise "Unexpected discriminant value"
            end
            Weapon.new(weapon_type)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@weapon_type)
        end
    end
    class Mine
        TAG = 2
        def initialize()
        end
        def self.read_from(stream)
            Mine.new()
        end
        def write_to(stream)
            stream.write_int(TAG)
        end
    end
end
