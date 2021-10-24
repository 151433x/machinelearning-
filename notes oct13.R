#simple smotthing averg method
# F_(t+h)= avg of all A's 
# no change method
#R: meanf(existing data,#periods)

# exp smoothing method: alpha,A_t,F_t
#f_(t+1)=alpha*A_t+(1-Alpha)*F_t method of trying to correct mistakes in previous predictions
# f_t= alpha*A_(t-1)+(1-alpha)*F_(t-1)
# alpha: between 0 and 1, minimize SSR (sum of squared residuals)
#f_1 is subjectively chosen, minimize SSR(sum of squared residuals)
# ses(,alpha=x,)

#Holt winters method or HW, capable of capture Level, trend and seasonality:only additive and multi
# basic holt, HW additive and HW multiplicative 
# f_(t+H)= L_t+h*T_t, basiic HW
# f_(t+h)= L_t+h*T_t+ seasonality component, additive
# f_(t+h)= (L_t+h*T_t) * seasonality index, multiplicative

