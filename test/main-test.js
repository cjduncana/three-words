'use strict';

global.XMLHttpRequest = require('xhr2');

const { expect } = require('chai');
const nock = require('nock');

nock.disableNetConnect();

const elm = require('./elm-test');
const failureFixture = require('./fixtures/failureResponse');
const successFixture = require('./fixtures/successResponse');

describe('Three Word API', () => {

  describe('#fromPosition', () => {

    it('should return with the three words when given a coordinates', () => {
      nock('https://api.what3words.com/v2', {
        reqheaders: { 'X-Api-Key': 'key' }
      })
        .get('/reverse')
        .query({ coords: '51.521251,-0.203586' })
        .reply(200, successFixture);

      return elm.getThreeWords('key', { lat: 51.521251, lng: -0.203586 })
        .then((response) => {
          expect(response).to.have.own.property('threeWords');
          expect(response.threeWords).to.equal('index.home.raft');
        });
    });
  });

  describe('#toPosition', () => {

    let mockApi;

    beforeEach(() => {
      mockApi = nock('https://api.what3words.com/v2', {
        reqheaders: { 'X-Api-Key': 'key' }
      }).get('/forward');
    });

      it('should return with the coordinates when given valid three words', () => {
        mockApi
          .query({ addr: 'index.home.raft' })
          .reply(200, successFixture);

        return elm.getPosition('key', 'index.home.raft')
          .then((response) => {
            expect(response).to.have.own.property('position');
            expect(response.position).to.deep.equal({
              latitude: 51.521251,
              longitude: -0.203586
            });
          });
      });

      it('should fail when given invalid three words', () => {
        mockApi
          .query({ addr: 'not.valid.address' })
          .reply(200, failureFixture);

        return elm.getPosition('key', 'not.valid.address')
          .then((response) => {
            expect(response).to.have.own.property('error');
            expect(response.error).to.deep.equal('Bad Payload: I ran into a `fail` decoder: The \'addr\' parameter is invalid or missing a partial or complete 3 word address');
          });
      });
  });

  it('should fail if wrong key was provided', () => {
    nock('https://api.what3words.com/v2', {
      reqheaders: { 'X-Api-Key': 'notTheKey' }
    })
      .get('/reverse')
      .query({ coords: '51.521251,-0.203586' })
      .reply(401);

    return elm.getThreeWords('notTheKey', {
      lat: 51.521251,
      lng: -0.203586
    })
      .then((response) => {
        expect(response).to.have.own.property('error');
        expect(response.error).to.equal('Bad Status: Unauthorized');
      });
  });

  it('should fail if what3words server fails', () => {
    nock('https://api.what3words.com/v2', {
      reqheaders: { 'X-Api-Key': 'key' }
    })
      .get('/reverse')
      .query({ coords: '51.521251,-0.203586' })
      .reply(500);

    return elm.getThreeWords('key', {
      lat: 51.521251,
      lng: -0.203586
    })
      .then((response) => {
        expect(response).to.have.own.property('error');
        expect(response.error).to.equal('Bad Status: Internal Server Error');
      });
  });
});
