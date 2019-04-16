from setuptools import setup

setup(name='pins',
      version='0.1.0',
      description='Track, Discover and Share Datasets',
      url='https://github.com/rstudio/pins',
      author='Javier Luraschi',
      author_email='javier@rstudio.com',
      license='Apache License 2.0',
      packages=['pins'],
      zip_safe=False,
      install_requires=[
          'cffi>=1.0.0',
          'pyyaml',
          'arrow',
      ],
      setup_requires=["cffi>=1.0.0"],
      cffi_modules=["pins/pins_build.py:ffibuilder"],
    )
