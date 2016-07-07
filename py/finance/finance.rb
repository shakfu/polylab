require 'enumerator'

def irr(cash_flows, iterations = 100)
   (1..iterations).inject do |rate,|
     p "=> #{rate}"
     npv = cash_flows.enum_with_index.inject {|(m,),(c,t)| m+c/(1.0+rate)**t}
     rate * (1 - npv / cash_flows.first)
   end
end

# From the example at http://en.wikipedia.org/wiki/Internal_rate_of_return
#irr([-100,+30,+35,+40,+45]) # => 0.170936863394991

irr([-100.0, 60.0, 50.0, 30.0])
