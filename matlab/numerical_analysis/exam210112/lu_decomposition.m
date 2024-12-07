function [L,U] = lu_decomposition(A)
  %LU_DECOMPOSITION Perform LU decomposition without pivoting on a matrix
  %   [L,U] = lu_decomposition(A)
  %
  %   Inputs  : A = input matrix
  %   Outputs : L = lower triangular matrix
  %             U = upper triangular matrix

  n = size(A, 1);

  U = A;
  L = eye(n);

  for (k = 1:n-1)   % cycle over the columns
    if (U(k,k) == 0)
      warning('Null diagonal element');
    end
    % cycle in the [k+1:end] times [k:end] submatrix
    for (i = k+1:n) 
      L(i,k) = U(i,k) / U(k,k);
      for (j = k:n)
	      U(i,j) = U(i,j) - L(i,k)*U(k,j);
      end
    end
  end
end
