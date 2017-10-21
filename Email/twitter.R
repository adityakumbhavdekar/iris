install.packages(XLConnect)
"XLConnect"
library(httr)
library(devtools)
library(twitter)
library(base64enc)
Consumer Key (API Key)	y0p7r2CZ5NJKoDYlz7NSphMLz
Consumer Secret (API Secret)	vaCgJCpKzSQAYhTPoURKaEAEE8AbuNEGSsplmd4hZES3lk0ScO
Access Token	868377331898826752-wBX6bXzsC9WUVSchpzPk4HwNKtapViQ
Access Token Secret	SLVTXCWNW2MAO77JItPzGSvUFiapQW1I9pRBKS5QNrpJ3
consumerkey="y0p7r2CZ5NJKoDYlz7NSphMLz"
consumersecret="vaCgJCpKzSQAYhTPoURKaEAEE8AbuNEGSsplmd4hZES3lk0ScO"
accesstoken="868377331898826752-wBX6bXzsC9WUVSchpzPk4HwNKtapViQ"
accesssecret="SLVTXCWNW2MAO77JItPzGSvUFiapQW1I9pRBKS5QNrpJ3"
some_tweets = searchTwitter("sachinabilliondreams", n=100, lang="en")
