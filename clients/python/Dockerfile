FROM python

RUN pip install numpy cython
RUN pip install torch==1.3.1+cpu torchvision==0.4.2+cpu -f https://download.pytorch.org/whl/torch_stable.html

COPY . /project
WORKDIR /project
