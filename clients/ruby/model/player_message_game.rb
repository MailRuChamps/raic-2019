require_relative 'custom_data'
require_relative 'versioned'
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
            action = Versioned.read_from(stream)
            ActionMessage.new(action)
        end
        def write_to(stream)
            stream.write_int(TAG)
            @action.write_to(stream)
        end
    end
end
