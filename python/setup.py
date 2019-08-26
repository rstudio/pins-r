from setuptools import setup

setup(name='pins',
      version='0.1.0',
      description='Pin, Discover and Share Resources',
      url='https://github.com/rstudio/pins',
      author='Javier Luraschi',
      author_email='javier@rstudio.com',
      license='Apache License 2.0',
      packages=['pins'],
      zip_safe=False,
      install_requires=[
          'cffi>=1.0.0',
          'feather-format',
          'pandas'
      ],
      setup_requires=["cffi>=1.0.0"],
      cffi_modules=["pins/pins_build.py:ffibuilder"],
    )
