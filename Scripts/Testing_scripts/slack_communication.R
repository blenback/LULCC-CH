
create_config_file(token = 'xapp-1-A049C8Z8ALW-4318425761910-7988238c82da2316a2c87bee2ad1962d6e42792243edd7517b12cce53a73056f',
  incoming_webhook_url = 'https://hooks.slack.com/services/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',
  channel = '#general',
  username = 'slackr',
  icon_emoji = 'tada')

slackr_setup(channel = '#general',
             incoming_webhook_url = 'https://hooks.slack.com/services/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')

#create a test promise
promise <- promises::future_promise(read.csv("Tools/Simulation_control.csv"))

#evaluate with then
then(promise,onFulfilled = function(value) {print("success")},
    onRejected = function(err) {print("Fail")})

slackr_bot('Wow!')
