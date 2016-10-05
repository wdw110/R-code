%% IEEESTD1057 正弦函数参数拟合算法  
  
clear;  
clc;  
  
% 待拟合数据  
M = 2 * pi;  
tn = 0 : 0.1 * pi : M;  
yn = 2 * sin(tn + 0.8);  
  
  
w = 1; % 初始权重  
sita = 0; % 初始相位  
  
index = 0;  
while(index < 1000)  
         
    yT = 1 / M * sum(yn);  
    alphaT = 1 / M * sum(cos(w * tn + sita));  
    alphaN = cos(w * tn + sita);  
    betaN = sin(w * tn + sita);  
    betaT = 1 / M * sum(betaN);  
      
    R = sum( (yn - yT) .* tn .* betaN ) / sum( (yn - yT) .* alphaN ) - sum( (alphaN - alphaT) .* tn .* betaN ) / sum( (alphaN - alphaT) .* alphaN );  
    S = sum( (yn - yT) .* betaN ) / sum( (yn - yT) .* alphaN ) - sum( (alphaN - alphaT) .* betaN ) / sum( (alphaN - alphaT) .* alphaN );  
      
    if( abs(R) < 1e-10 && abs(S) < 1e-10 )  
        break;  
    end  
      
    a11 = sum( (betaN .* tn .* (alphaN - alphaT)) ) * sum( (alphaN .* tn .* (betaN - betaT)) ) / sum(alphaN .* (alphaN - alphaT)) ^ 2 - ...  
           sum(alphaN .* (alphaN - alphaT)) * sum(betaN .* tn .^ 2 .* (betaN - betaT)) / sum(alphaN .* (alphaN - alphaT)) ^ 2;  
         
    a12 = sum( (betaN .* tn .* (alphaN - alphaT)) ) * sum( (alphaN .* (betaN - betaT)) ) / sum(alphaN .* (alphaN - alphaT)) ^ 2 - ...  
           sum(alphaN .* (alphaN - alphaT)) * sum(betaN .* tn .* (betaN - betaT)) / sum(alphaN .* (alphaN - alphaT)) ^ 2;  
  
    a21 = sum( (betaN .* (alphaN - alphaT)) ) * sum( (alphaN .* tn .* (betaN - betaT)) ) / sum(alphaN .* (alphaN - alphaT)) ^ 2 - ...  
           sum(alphaN .* (alphaN - alphaT)) * sum(betaN .* tn .* (betaN - betaT)) / sum(alphaN .* (alphaN - alphaT)) ^ 2;  
  
    a22 = sum( (betaN .* (alphaN - alphaT)) ) * sum( (alphaN .* (betaN - betaT)) ) / sum(alphaN .* (alphaN - alphaT)) ^ 2 - ...  
           sum(alphaN .* (alphaN - alphaT)) * sum(betaN .* (betaN - betaT)) / sum(alphaN .* (alphaN - alphaT)) ^ 2;  
         
         
  
    psai = w + (a22 * R - a12 * S) / (a11 * a22 - a12 * a21);  
    fai = sita + (a11 * S - a21 * R) / (a11 * a22 - a12 * a21);  
  
    w = psai;  
    sita = fai;  
      
    A = sum( (yn - yT) .* (alphaN + betaN + betaN .* tn) ) / sum( (alphaN - alphaT) .* (alphaN + betaN + betaN .* tn) );  
    C = yT - A * alphaT;  
      
    index = index + 1  
    errorR(index) = sqrt( 1 / M * sum( yn .^ 2 ) + A ^ 2 / M * sum( alphaN .^ 2) - 2 * A / M * sum( alphaN .* yn) + C ^ 2 - 2 * C * yT + 2 * A * C * alphaT );  
    wR(index) = w;  
    sitaR(index) = sita;  
end  
  
  
  
figure;  
hold on ; plot(tn , yn , '-.or'); % 待拟合数据  
rxN = 0 : 0.01 : M;  
ryN = A * cos(w * rxN + sita) + C; % 拟合结果  
hold on ; plot(rxN ,  ryN , '-g');  
legend('待拟合数据' , '拟合结果');  
title('拟合结果');  
  
figure;  
hold on ; plot(1 : index , wR , 'r');  
hold on ; plot(1 : index , sitaR , 'b');  
legend('角度探测' , '相位探测');  
title('探测参数 角度与相位');  
  
figure;  
hold on ; plot(1 : index , errorR , 'r');  
title('迭代误差');  
