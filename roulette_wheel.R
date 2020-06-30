roulette = function(bet, amount){
  pocket = sample(37, 1)
  pocket_odds = 37 - 1
  if(bet == pocket){
    amount * pocket_odds
  }else{
    -amount
  }
}

#change the number of iteration
playRoulette = function(bet, amount, spins){
iter = spins
outs = map_dbl(1:iter, function(x){roulette(bet = bet, amount = amount)})
expected_return = (sum(outs)/(amount*spins))*100
print(paste(iter, 'Spins of Roulette Wheel on', bet, 'gives' ,'the expected return', '=', paste(expected_return, '%', sep = '')))
}


playRoulette(10, 1, 10000000)

