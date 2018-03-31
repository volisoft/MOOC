function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%
coefficients = [0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30];

[p,q] = meshgrid(coefficients, coefficients);
pairs = [p(:) q(:)];
f1 = 0;
for i = 1:size(pairs, 1)
    C_current = pairs(i, 1); sigma_current = pairs(i, 2);
    
    model = svmTrain(X, y, C_current, @(x1, x2) gaussianKernel(x1, x2, sigma_current));
    predictions = svmPredict(model, Xval);
    
    % calculate f1 score
    actual_positives = sum(yval);
    predicted_positives = sum(predictions);
    true_positives = sum(predictions & yval);
    
    recall = true_positives/actual_positives;
    precision = true_positives/predicted_positives;
    f1_current = 2 * precision * recall / (precision + recall);
    
    % update C and sigma if required
    if f1_current > f1
        f1 = f1_current;
        C = C_current; sigma = sigma_current;
    end
end

% =========================================================================

end
