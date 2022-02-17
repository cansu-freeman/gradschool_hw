def bondPV(years, ytm, r, F):
    '''
    This function inputs a bond's number of years to maturity, 
    the yield-to-maturity, and coupon rate, and the bond's face value, 
    and returns the bond's present value.
    
    Input Variables: 
    years: years to maturity
    aytm:  annual yield to maturity
    r:     annual coupon rate
    F:     face value aka principal
    
    Other Variables:
    dr: discount rate
    CP: coupon payment  
    N:  no. of 6 month periods to maturity   
    '''
    
    dr = .5*ytm
    CP = (r*F)/2
    N = years*2  
    
    annCoupPV = 0
    for t in range(1, N+1):
        annCoupPV = annCoupPV + CP/((1+dr)**t)
    
    principalPV = F/(1+dr)**N
    
    PV = annCoupPV + principalPV
    
    return PV
  
  
  
