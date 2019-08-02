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

	{raboter, ".*", {git, "https://github.com/Robyroc/raboter", {branch, "master"}}}


##### Technology:
- Erlang

## License
Erlang telegram bot is released under the [MIT License](http://www.opensource.org/licenses/MIT).