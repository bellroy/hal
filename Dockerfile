FROM amazon/aws-sam-cli-emulation-image-provided:rapid-1.17.0
RUN /usr/sbin/groupadd stack
RUN /usr/sbin/useradd stack -g stack
RUN yum -y install bash gcc gmp-devel zlib-devel
USER stack
RUN mkdir ~/.stack
ENV PATH=/usr/sbin:$HOME/.local/bin:$PATH
RUN mkdir -p ~/.local/bin
RUN curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
USER root
