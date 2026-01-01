import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt


def main():
    # サンプルデータ生成
    np.random.seed(42)
    X = np.random.rand(100, 1) * 10  # 0-10の範囲で100個のデータ
    y = 2.5 * X.flatten() + 3 + np.random.randn(100) * 2  # y = 2.5x + 3 + ノイズ

    # 訓練データとテストデータに分割
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # モデルの作成と学習
    model = LinearRegression()
    model.fit(X_train, y_train)

    # 予測
    y_pred = model.predict(X_test)

    # 評価
    mse = mean_squared_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)

    print(f"係数 (傾き): {model.coef_[0]:.4f}")
    print(f"切片: {model.intercept_:.4f}")
    print(f"平均二乗誤差 (MSE): {mse:.4f}")
    print(f"決定係数 (R²): {r2:.4f}")

    # 可視化
    plt.figure(figsize=(10, 6))
    plt.scatter(X_train, y_train, color="blue", alpha=0.5, label="訓練データ")
    plt.scatter(X_test, y_test, color="green", alpha=0.5, label="テストデータ")
    plt.plot(X_test, y_pred, color="red", linewidth=2, label="回帰直線")
    plt.xlabel("X")
    plt.ylabel("y")
    plt.title("線形回帰")
    plt.legend()
    plt.savefig("regression_result.png", dpi=150)
    plt.show()


if __name__ == "__main__":
    main()
