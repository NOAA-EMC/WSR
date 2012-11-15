      SUBROUTINE DMRRRR (NRA, NCA, A, LDA, NRB, NCB, B, LDB, NRC, NCC,
     &                   C, LDC)

      integer nra,nca,lda,nrb,ncb,ldb,nrc,ncc,ldc
      double precision a(nra,nca),b(nrb,ncb),c(nrc,ncc)
c     double precision a(lda,*),b(ldb,*),c(ldc,*)
      double precision alpha, beta 

      if (nca.ne.nrb) print*, '#Columns of A must equal #rows of B'
      if (ncb.ne.ncc) print*, '#Columns of B must equal #columns of C'
      if (nra.ne.nrc) print*, '#Rows of A must equal #rows of C'
       
      alpha=1.0d0
      beta=0.0d0
      call DGEMM('n','n',nra,ncb,nca,alpha,a,lda,b,
     &              ldb,beta,c,ldc)


c      do 10 i=1,nrc
c      do 10 j=1,ncc
c         c(i,j)=0.0
c         do 15 k=1,nca
c            c(i,j)=c(i,j)+a(i,k)*b(k,j)
c15       continue
c10    continue

      return
      end


      SUBROUTINE DEVCSF (N, A, LDA, EVAL, EVEC, LDEVEC)

      integer n,lda,ldevec
      double precision a(lda,lda),eval(lda),evec(lda,lda)
      double precision diag(lda),sdiag(lda)

      external tred2, tql2

      call tred2(lda,lda,a,diag,sdiag,evec)
      call tql2(lda,lda,diag,sdiag,evec,ierr)

      do 20 l=1,lda
         eval(l)=diag(l)
20    continue

      return
      end
