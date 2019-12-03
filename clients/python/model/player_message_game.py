class PlayerMessageGame:
    @staticmethod
    def read_from(stream):
        discriminant = stream.read_int()
        if discriminant == CustomDataMessage.TAG:
            return PlayerMessageGame.CustomDataMessage.read_from(stream)
        if discriminant == ActionMessage.TAG:
            return PlayerMessageGame.ActionMessage.read_from(stream)
        raise Exception("Unexpected discriminant value")

from .custom_data import CustomData
class CustomDataMessage(PlayerMessageGame):
    TAG = 0
    def __init__(self, data):
        self.data = data
    @staticmethod
    def read_from(stream):
        data = CustomData.read_from(stream)
        return CustomDataMessage(data)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        self.data.write_to(stream)
    def __repr__(self):
        return "CustomDataMessage(" + \
            repr(self.data) + \
            ")"
PlayerMessageGame.CustomDataMessage = CustomDataMessage
from .versioned import Versioned
class ActionMessage(PlayerMessageGame):
    TAG = 1
    def __init__(self, action):
        self.action = action
    @staticmethod
    def read_from(stream):
        action = Versioned.read_from(stream)
        return ActionMessage(action)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        self.action.write_to(stream)
    def __repr__(self):
        return "ActionMessage(" + \
            repr(self.action) + \
            ")"
PlayerMessageGame.ActionMessage = ActionMessage
