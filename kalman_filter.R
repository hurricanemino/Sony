
# dlmの使い方

library(dlm)

# 使うデータNileのプロット
plot(Nile, type="o",col=8)


#　Step1 モデルの型を決める
build.1 <- function(theta){
  dlmModPoly(order=1, dV=exp(theta[1]), dW=exp(theta[2]))
}

#　Step2　パラメタ推定
fit.1 <- dlmMLE(Nile, parm=c(1, 1), build.1)
# 最尤法で最も良さそうなパラメタを推定する
fit.1
fit.1$par					#　推定されたパラメタ

#　推定された分散を使って、モデルを組みなおす
mod.Nile <- build.1(fit.1$par)		
mod.Nile

#　Step3　フィルタリング
NileFilt <- dlmFilter(Nile, mod.Nile)	

#　Step4　スムージング
NileSmooth <- dlmSmooth(NileFilt)	# スムージングする

NileFilt$m					# 推定された状態
NileSmooth$s				# スムージングされた結果

#　図を描く
plot(Nile, type="o", col=8, ylab="", main="Nile Filtering")
lines(dropFirst(NileFilt$m), col=2, lwd=2)		# 状態

plot(Nile, type="o", col=8, ylab="", main="Nile Smoothing")
lines(dropFirst(NileSmooth$s), col=4, lwd=2)	#　Somooting


