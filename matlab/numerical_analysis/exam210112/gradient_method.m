function [x,niter,incr] = gradient_method(A, b, P, toll, nitmax)
%
% The script implements the stationary Gradient method to solve a linear
% system. The script requires in input the matrix A, the known vector, the
% preconditioner, the tolerance and the maximum allowed
% number of iterations. In output, it returns the computed solution x, 
% the error vector (err) and the iteration vector (niter)
%
% [x,err,niter] = gradient_method(A, b, P, toll, nitmax)
%

% initialize the method
n      = max(size(b)); % matrix order
x      = zeros(n,1);   % vector into the solution will be allocated 
r      = b - A*x;      % residual 

% inizialization of the while paramters
incr = inf;
niter  = 0 ;

%
while ((incr > toll) && (niter < nitmax))
% updating the iterator
      niter = niter + 1;
% solve the linear system 
      z     = P \ r;
% compute the acceleration parameter
      alpha = (z'*r)/(z'*A*z);
% update the solution
      x     = x + alpha*z;			
% update the residual
			r     = r - alpha*A*z;
% calculation of the error (normalized residual)
      incr = norm(alpha * z, 2);
end

return
