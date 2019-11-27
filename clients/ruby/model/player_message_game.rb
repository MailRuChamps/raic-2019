require_relative 'custom_data'
require_relative 'unit_action'
class PlayerMessageGame
    def self.read_from(stream)
        discriminant = stream.read_int()
        if discriminant == PlayerMessageGame::CustomDataMessage::TAG
            return PlayerMessageGame::CustomDataMessage.read_from(stream)
        end
        if discriminant == PlayerMessageGame::ActionMessage::TAG
            return PlayerMessageGame::ActionMessage.read_from(stream)
        end
        raise "Unexpected discriminant value"
    end

    class CustomDataMessage
        TAG = 0
        attr_accessor :data
        def initialize(data)
            @data = data
        end
        def self.read_from(stream)
            data = CustomData.read_from(stream)
            CustomDataMessage.new(data)
        end
        def write_to(stream)
            stream.write_int(TAG)
            @data.write_to(stream)
        end
    end
    class ActionMessage
        TAG = 1
        attr_accessor :action
        def initialize(action)
            @action = action
        end
        def self.read_from(stream)
            action = Hash.new
            stream.read_int().times do |_|
                action_key = stream.read_int()
                action_value = UnitAction.read_from(stream)
                action[action_key] = action_value
            end
            ActionMessage.new(action)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@action.length())
            @action.each do |key, value|
                stream.write_int(key)
                value.write_to(stream)
            end
        end
    end
end
