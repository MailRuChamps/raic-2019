require_relative 'unit_action'
class Versioned
    attr_accessor :inner
    def initialize(inner)
        @inner = inner
    end
    def self.read_from(stream)
        inner = Hash.new
        stream.read_int().times do |_|
            inner_key = stream.read_int()
            inner_value = UnitAction.read_from(stream)
            inner[inner_key] = inner_value
        end
        Versioned.new(inner)
    end
    def write_to(stream)
        stream.write_int(43981)
        stream.write_int(@inner.length())
        @inner.each do |key, value|
            stream.write_int(key)
            value.write_to(stream)
        end
    end
end
