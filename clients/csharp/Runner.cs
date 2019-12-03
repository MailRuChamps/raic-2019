using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Net.Sockets;
using System.Text;

namespace AiCup2019
{
    public class Runner
    {
        private BinaryReader reader;
        private BinaryWriter writer;
        public Runner(string host, int port, string token)
        {
            var client = new TcpClient(host, port) { NoDelay = true };
            var stream = new BufferedStream(client.GetStream());
            reader = new BinaryReader(stream);
            writer = new BinaryWriter(stream);
            var tokenData = System.Text.Encoding.UTF8.GetBytes(token);
            writer.Write(tokenData.Length);
            writer.Write(tokenData);
            writer.Flush();
        }
        public void Run()
        {
            var myStrategy = new MyStrategy();
            var debug = new Debug(writer);
            while (true)
            {
                Model.ServerMessageGame message = Model.ServerMessageGame.ReadFrom(reader);
                if (!message.PlayerView.HasValue)
                {
                    break;
                }
                Model.PlayerView playerView = message.PlayerView.Value;
                var actions = new Dictionary<int, Model.UnitAction>();
                foreach (var unit in playerView.Game.Units)
                {
                    if (unit.PlayerId == playerView.MyId)
                    {
                        actions.Add(unit.Id, myStrategy.GetAction(unit, playerView.Game, debug));
                    }
                }
                new Model.PlayerMessageGame.ActionMessage(new Model.Versioned(actions)).WriteTo(writer);
                writer.Flush();
            }
        }
        public static void Main(string[] args)
        {
            string host = args.Length < 1 ? "127.0.0.1" : args[0];
            int port = args.Length < 2 ? 31001 : int.Parse(args[1]);
            string token = args.Length < 3 ? "0000000000000000" : args[2];
            new Runner(host, port, token).Run();
        }
    }
}