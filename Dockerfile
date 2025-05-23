# ベースとするイメージ
FROM rocker/tidyverse:4.2

# OS環境を日本語ロケールに切り替える

ENV LANG ja_JP.UTF-8
ENV LC_ALL ja_JP.UTF-8
RUN sed -i '$d' /etc/locale.gen \
  && echo "ja_JP.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen ja_JP.UTF-8 \
    && /usr/sbin/update-locale LANG=ja_JP.UTF-8 LANGUAGE="ja_JP:ja"
RUN /bin/bash -c "source /etc/default/locale"
RUN ln -sf  /usr/share/zoneinfo/Asia/Tokyo /etc/localtime

# 日本語フォントをインストールする
RUN apt-get update && apt-get install -y \
  fonts-ipaexfont \
  fonts-noto-cjk
# Rパッケージ用のインストール
RUN apt-get update && apt-get install -y \
  libbz2-dev \
  liblzma-dev\
  python
# RNA-seq解析用パッケージのインストール．
RUN apt-get update && apt-get install -y \
  rna-star \
  fastqc \
  trimmomatic \
  rsem