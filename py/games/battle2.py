import gambit



g = gambit.Game.new_table([2,2])

g.title = 'Battle of the Sexes'


g.players[0].label = 'player1'
g.players[1].label = 'player2'

g.players[0].strategies[0].label = 'Ballet'
g.players[0].strategies[1].label = 'Fight'

# m = [
#    [(1,2), (0,0)],
#    [(0,0), (2,1)]
#]


g[0,0][0] = 1
g[0,0][1] = 2
g[0,1][0] = 0
g[0,1][1] = 0


g.players[1].strategies[0].label = 'Ballet'
g.players[1].strategies[1].label = 'Fight'

g[1,0][0] = 0
g[1,0][1] = 0
g[1,1][0] = 2
g[1,1][1] = 1

print 'mixed profile'
p = g.mixed_strategy_profile()
print 'p.payoff(g.players[0])'
print p.payoff(g.players[0])

print 'p.payoff(g.players[1])'
print p.payoff(g.players[1])

print 'pure solver'
pure_solver = gambit.nash.ExternalEnumPureSolver()
print pure_solver.solve(g)

print 'mixed solver'
mixed_solver = gambit.nash.ExternalEnumMixedSolver()
print mixed_solver.solve(g)
