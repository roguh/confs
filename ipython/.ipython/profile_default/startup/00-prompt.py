import sys

if sys.version_info.major >= 3:
    import datetime

    from IPython import get_ipython
    from IPython.terminal.prompts import Prompts, Token

    class PromptWithTimestamp(Prompts):
        def in_prompt_tokens(self, cli=None):
            return [
                (Token, datetime.datetime.now().strftime("%H:%M:%S") + " "),
            ] + super().in_prompt_tokens()

        def out_prompt_tokens(self):
            return [
                (Token, datetime.datetime.now().strftime("%H:%M:%S") + " "),
            ] + super().out_prompt_tokens()

    get_ipython().prompts = PromptWithTimestamp(get_ipython())
