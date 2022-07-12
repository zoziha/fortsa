# 时间序列分析 Fortran 接口包

> 中文 | [English](./README.md)

## Fortran 的单变量时间序列分析和 ARIMA 建模包

`fortsa`是基于 [rafat/ctsa](https://github.com/rafat/ctsa) 的单变量时间序列分析的 Fortran 接口包。

`ctsa` 是一个用于单变量时间序列分析的 C 软件包。

## 开始

```sh
git clone https://github.com/zoziha/fortsa.git
cd fortsa
```
### 依赖

- [Fortran-lang/fpm][1] >= 0.6.0：用于构建代码；
- [GNU/GCC][2] >= 9.4.0：用于编译 C、Fortran 代码。

[1]: https://github.com/fortran-lang/fpm
[2]: https://gcc.gnu.org/

### 使用 [Fortran-lang/fpm][1] 构建

Fortran 包管理器 (fpm) 是 Fortran 的包管理器和构建系统。<br>
可以使用提供的 `fpm.toml` 进行构建：

```sh
fpm build --profile release
```

要在 fpm 项目中使用 `fortsa`，请将以下内容添加到 `fpm.toml` 文件中：

```toml
[dependencies]
fortsa = { git = "https://gitee.com/fortran-ipd/fortsa" }
```
## `ctsa` 文档

|**[自动 ARIMA](https://github.com/rafat/ctsa/wiki/AUTO-ARIMA)**|自动 ARIMA 类 + 示例 |
|:------------------------------------------------:|:----------------------------------:|
|**[SARIMAX](https://github.com/rafat/ctsa/wiki/SARIMAX/)**| SARIMAX 课程 + 示例 |
|**[ARIMA](https://github.com/rafat/ctsa/wiki/ARIMA)**| ARIMA 类 + 示例 |
|**[季节性 ARIMA](https://github.com/rafat/ctsa/wiki/SARIMA)**|季节性 ARIMA 课程 + 示例 |
|**[AR](https://github.com/rafat/ctsa/wiki/AR)** | AR 类 + 示例 |
|**[ACF](https://github.com/rafat/ctsa/wiki/ACF)** |自协方差、自相关和偏自相关 + 示例|
|**[参考资料](https://github.com/rafat/ctsa/wiki/References)**|参考文献（列表正在更新） |

维基可在

[ctsa/wiki](https://github.com/rafat/ctsa/wiki)

许可证：BSD 3-条款，检查许可证文件

对于 C 例程，请联系 rafat.hsn@gmail.com。
Fortran 例程请联系 zuo.zhihua@qq.com。

## 链接

1. [rafat/ctsa](https://github.com/rafat/ctsa)
2. [fortran-lang](https://fortran-lang.org/learn/)
3. [fortran-lang/fpm](https://github.com/fortran-lang/fpm)