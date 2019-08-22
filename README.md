## Erlang telegram bot

##### Instruction
 - First of all you need a token. To get the token follow the instructions here [how-do-i-create-a-bot] (https://core.telegram.org/bots/#3-how-do-i-create-a-bot).
 - Put the token in a file called "token.tok" inside the working repository.
 - Implement the method run_command/2 in one of the modules of your application. The method will take care of all the incoming messages. The method accepts 2 parameters: 
	 - the first one is the chat_id of the incoming message, it will identify the sender and can be used to answer;
	 - the second one is the text of the received message.
 - run this command somewhere early in your code (root supervisor):
	`application:set_env(raboter, target, <MODULE_NAME>).`
	where <MODULE_NAME> must be replaced with the name of the module containing the run_command/2 function discussed above.	

##### Include:
To include this dependency add the following line to the rebar config file. Note that this version supports rebar3 and it's highly suggested to use it.

	{raboter, ".*", {git, "https://github.com/radist101/raboter", {branch, "master"}}}

##### Usage:
To send a message call the function `raboter:send_message(ChatId, Text).` where ChatId is a value identifying the conversation, Text is the message to be sent. If you want more help you can find an example of a bot [here](https://github.com/Robyroc/repeato).

##### Test:
To check if the plugin is correctly reading received messages the following command can be run while in the cloned repository:

    ./rebar3 shell

then send a message to the created bot and check for an answer: if the answer is in the form of:

> Message is being ignored -> Message: << MESSAGE >> check if the environment variable is set correctly

then the problem is in the last step of the instructions.
If there is no answer after few seconds, double-check the token.tok location, its correctness and your internet connection. 

##### Issues:
The plugin uses jiffy ([https://github.com/davisp/jiffy](https://github.com/davisp/jiffy)) to decode JSON answers created by the telegram bot API. jiffy may create some problems when working on windows: we suggest to move the program to a linux environment.
##### Technology:
- Erlang

## License
Erlang telegram bot is released under the [MIT License](http://www.opensource.org/licenses/MIT).