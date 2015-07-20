# RCONClient
An RCON client to communicate with a Minecraft server written in Haskell.
Refer [https://developer.valvesoftware.com/wiki/Source_RCON_Protocol] to understand more about RCON protocol

# Introduction

This includes the basic functionalities for;
- Establish a connection with the Minecraft server
- Parse the RCON request into a byte array
- Receive the response and then display its content

# How to use

1. Clone the Git repository using 'git clone https://github.com/Dananji/RCONClient.git'
2. Open the terminal, and browse into the cloned project folder
3. Create the .exe file with the 'ghc -o rconclient RCONClient.hs' command
4. Then execute 'rconclient.exe <"host"> <port>', example rconclient.exe "127.0.0.1" 25575